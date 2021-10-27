{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Godx.DbSync.Era.Sophie.Genesis
  ( insertValidateGenesisDist
  ) where

import           Godx.Prelude

import           Godx.BM.Trace (Trace, logInfo)

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (newExceptT)

import qualified Godx.Db as DB

import qualified Godx.DbSync.Era.Sophie.Generic.Util as Generic
import           Godx.DbSync.Era.Util (liftLookupFail)
import           Godx.Sync.Error
import           Godx.Sync.Util

import qualified Godx.Ledger.Address as Ledger
import qualified Godx.Ledger.Coin as Ledger
import           Godx.Ledger.Era (Crypto)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime (..))
import qualified Data.Time.Clock as Time

import           Database.Persist.Sql (SqlBackend)

import           Shardagnostic.Consensus.Godx.Block (StandardCrypto, StandardSophie)
import           Shardagnostic.Consensus.Sophie.Node (SophieGenesis (..))

import qualified Sophie.Spec.Ledger.Genesis as Sophie
import           Sophie.Spec.Ledger.Scripts ()
import qualified Sophie.Spec.Ledger.TxBody as Sophie
import qualified Sophie.Spec.Ledger.UTxO as Sophie

-- | Idempotent insert the initial Genesis distribution transactions into the DB.
-- If these transactions are already in the DB, they are validated.
insertValidateGenesisDist
    :: SqlBackend -> Trace IO Text -> Text -> SophieGenesis StandardSophie
    -> ExceptT SyncNodeError IO ()
insertValidateGenesisDist backend tracer networkName cfg = do
    -- Setting this to True will log all 'Persistent' operations which is great
    -- for debugging, but otherwise *way* too chatty.
    if False
      then newExceptT $ DB.runDbtbcoLogging backend tracer insertAction
      else newExceptT $ DB.runDbtbcoNoLogging backend insertAction
  where
    insertAction :: (MonadBaseControl IO m, MonadIO m) => ReaderT SqlBackend m (Either SyncNodeError ())
    insertAction = do
      ebid <- DB.queryBlockId (configGenesisHash cfg)
      case ebid of
        Right bid -> validateGenesisDistribution tracer networkName cfg bid
        Left _ ->
          runExceptT $ do
            liftIO $ logInfo tracer "Inserting Sophie Genesis distribution"
            emeta <- lift DB.queryMeta
            case emeta of
              Right _ -> pure () -- Metadata from Cole era already exists. TODO Validate metadata.
              Left _ -> do
                count <- lift DB.queryBlockCount
                when (count > 0) $
                  dbSyncNodeError $ "Sophie.insertValidateGenesisDist: Genesis data mismatch. count " <> textShow count
                void . lift $ DB.insertMeta $
                            DB.Meta
                              { DB.metaStartTime = configStartTime cfg
                              , DB.metaNetworkName = networkName
                              }
                -- Insert an 'artificial' Genesis block (with a genesis specific slot leader). We
                -- need this block to attach the genesis distribution transactions to.
                -- It would be nice to not need this artificial block, but that would
                -- require plumbing the Genesis.Config into 'insertColeBlockOrEBB'
                -- which would be a pain in the neck.
                slid <- lift . DB.insertSlotLeader $
                                DB.SlotLeader
                                  { DB.slotLeaderHash = genesisHashSlotLeader cfg
                                  , DB.slotLeaderPoolHashId = Nothing
                                  , DB.slotLeaderDescription = "Sophie Genesis slot leader"
                                  }
                bid <- lift . DB.insertBlock $
                          DB.Block
                            { DB.blockHash = configGenesisHash cfg
                            , DB.blockEpochNo = Nothing
                            , DB.blockSlotNo = Nothing
                            , DB.blockEpochSlotNo = Nothing
                            , DB.blockBlockNo = Nothing
                            , DB.blockPreviousId = Nothing
                            , DB.blockSlotLeaderId = slid
                            , DB.blockSize = 0
                            , DB.blockTime = configStartTime cfg
                            , DB.blockTxCount = fromIntegral (length $ genesisTxos cfg)
                            -- Genesis block does not have a protocol version, so set this to '0'.
                            , DB.blockProtoMajor = 0
                            , DB.blockProtoMinor = 0
                            -- Sophie specific
                            , DB.blockVrfKey = Nothing
                            , DB.blockOpCert = Nothing
                            , DB.blockOpCertCounter = Nothing
                            }
                lift $ mapM_ (insertTxOuts bid) $ genesisUtxOs cfg
                liftIO . logInfo tracer $ "Initial genesis distribution populated. Hash "
                                <> renderByteArray (configGenesisHash cfg)

                supply <- lift DB.queryTotalSupply
                liftIO $ logInfo tracer ("Total genesis supply of Godx: " <> DB.renderGodx supply)

-- | Validate that the initial Genesis distribution in the DB matches the Genesis data.
validateGenesisDistribution
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Text -> SophieGenesis StandardSophie -> DB.BlockId
    -> ReaderT SqlBackend m (Either SyncNodeError ())
validateGenesisDistribution tracer networkName cfg bid =
  runExceptT $ do
    liftIO $ logInfo tracer "Validating Genesis distribution"
    meta <- liftLookupFail "Sophie.validateGenesisDistribution" DB.queryMeta

    when (DB.metaStartTime meta /= configStartTime cfg) $
      dbSyncNodeError $ Text.concat
            [ "Sophie: Mismatch chain start time. Config value "
            , textShow (configStartTime cfg)
            , " does not match DB value of ", textShow (DB.metaStartTime meta)
            ]

    when (DB.metaNetworkName meta /= networkName) $
      dbSyncNodeError $ Text.concat
            [ "Sophie.validateGenesisDistribution: Provided network name "
            , networkName
            , " does not match DB value "
            , DB.metaNetworkName meta
            ]

    txCount <- lift $ DB.queryBlockTxCount bid
    let expectedTxCount = fromIntegral $length (genesisTxos cfg)
    when (txCount /= expectedTxCount) $
      dbSyncNodeError $ Text.concat
              [ "Sophie.validateGenesisDistribution: Expected initial block to have "
              , textShow expectedTxCount
              , " but got "
              , textShow txCount
              ]
    totalSupply <- lift DB.queryGenesisSupply
    let expectedSupply = configGenesisSupply cfg
    when (expectedSupply /= totalSupply) $
      dbSyncNodeError  $ Text.concat
         [ "Sophie.validateGenesisDistribution: Expected total supply to be "
         , textShow expectedSupply
         , " but got "
         , textShow totalSupply
         ]
    supply <- lift DB.queryGenesisSupply
    liftIO $ do
      logInfo tracer "Initial genesis distribution present and correct"
      logInfo tracer ("Total genesis supply of Godx: " <> DB.renderGodx supply)

-- -----------------------------------------------------------------------------

insertTxOuts
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.BlockId -> (Sophie.TxIn (Crypto StandardSophie), Sophie.TxOut StandardSophie)
    -> ReaderT SqlBackend m ()
insertTxOuts blkId (Sophie.TxIn txInId _, txOut) = do
  -- Each address/value pair of the initial coin distribution comes from an artifical transaction
  -- with a hash generated by hashing the address.
  txId <- DB.insertTx $
            DB.Tx
              { DB.txHash = Generic.unTxHash txInId
              , DB.txBlockId = blkId
              , DB.txBlockIndex = 0
              , DB.txOutSum = Generic.coinToDbIsaac (txOutCoin txOut)
              , DB.txFee = DB.DbIsaac 0
              , DB.txDeposit = 0
              , DB.txSize = 0 -- Genesis distribution address to not have a size.
              , DB.txInvalidHereafter = Nothing
              , DB.txInvalidBefore = Nothing
              , DB.txValidContract = True
              , DB.txScriptSize = 0
              }
  void . DB.insertTxOut $
            DB.TxOut
              { DB.txOutTxId = txId
              , DB.txOutIndex = 0
              , DB.txOutAddress = Generic.renderAddress (txOutAddress txOut)
              , DB.txOutAddressRaw = Ledger.serialiseAddr (txOutAddress txOut)
              , DB.txOutAddressHasScript = hasScript (txOutAddress txOut)
              , DB.txOutPaymentCred = Generic.maybePaymentCred (txOutAddress txOut)
              , DB.txOutStakeAddressId = Nothing -- No stake addresses in Sophie Genesis
              , DB.txOutValue = Generic.coinToDbIsaac (txOutCoin txOut)
              , DB.txOutDataHash = Nothing -- No data hash in Sophie Genesis
              }
  where
    txOutAddress :: Sophie.TxOut StandardSophie -> Ledger.Addr StandardCrypto
    txOutAddress (Sophie.TxOut out _) = out

    txOutCoin :: Sophie.TxOut StandardSophie -> Ledger.Coin
    txOutCoin (Sophie.TxOut _ coin) = coin

    hasScript addr = maybe False Generic.hasCredScript (Generic.getPaymentCred addr)

-- -----------------------------------------------------------------------------

configGenesisHash :: SophieGenesis StandardSophie -> ByteString
configGenesisHash _ =  BS.take 28 ("GenesisHash " <> BS.replicate 28 '\0')

genesisHashSlotLeader :: SophieGenesis StandardSophie -> ByteString
genesisHashSlotLeader = configGenesisHash

configGenesisSupply :: SophieGenesis StandardSophie -> DB.Godx
configGenesisSupply =
  DB.word64ToGodx . fromIntegral . sum . map (Ledger.unCoin . snd) . genesisTxoAssocList

genesisTxos :: SophieGenesis StandardSophie -> [Sophie.TxOut StandardSophie]
genesisTxos = map (uncurry Sophie.TxOut) . genesisTxoAssocList

genesisTxoAssocList :: SophieGenesis StandardSophie -> [(Ledger.Addr StandardCrypto, Ledger.Coin)]
genesisTxoAssocList =
    map (unTxOut . snd) . genesisUtxOs
  where
    unTxOut :: Sophie.TxOut StandardSophie -> (Ledger.Addr StandardCrypto, Ledger.Coin)
    unTxOut (Sophie.TxOut addr amount) = (addr, amount)

genesisUtxOs :: SophieGenesis StandardSophie -> [(Sophie.TxIn (Crypto StandardSophie), Sophie.TxOut StandardSophie)]
genesisUtxOs =
    Map.toList . unUTxO . Sophie.genesisUTxO
  where
    -- Sigh!
    unUTxO :: Sophie.UTxO StandardSophie -> Map (Sophie.TxIn (Crypto StandardSophie)) (Sophie.TxOut StandardSophie)
    unUTxO (Sophie.UTxO m) = m

configStartTime :: SophieGenesis StandardSophie -> UTCTime
configStartTime = roundToMillseconds . Sophie.sgSystemStart

roundToMillseconds :: UTCTime -> UTCTime
roundToMillseconds (UTCTime day picoSecs) =
    UTCTime day (Time.picosecondsToDiffTime $ 1000000 * (picoSeconds `div` 1000000))
  where
    picoSeconds :: Integer
    picoSeconds = Time.diffTimeToPicoseconds picoSecs
