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

module Bcc.DbSync.Era.Cole.Genesis
  ( insertValidateGenesisDist
  ) where

import           Bcc.Prelude

import           Bcc.BM.Trace (Trace, logInfo)
import qualified Bcc.Binary as Binary
import qualified Bcc.Chain.Common as Cole
import qualified Bcc.Chain.Genesis as Cole
import qualified Bcc.Chain.UTxO as Cole
import qualified Bcc.Crypto as Crypto (Hash, fromCompactRedeemVerificationKey,
                   serializeCborHash)

import qualified Bcc.Db as DB
import           Bcc.DbSync.Era.Util (liftLookupFail)
import qualified Bcc.Sync.Era.Cole.Util as Cole
import           Bcc.Sync.Error
import           Bcc.Sync.Util

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (newExceptT)

import qualified Data.ByteString.Char8 as BS
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Database.Persist.Sql (SqlBackend)

-- | Idempotent insert the initial Genesis distribution transactions into the DB.
-- If these transactions are already in the DB, they are validated.
insertValidateGenesisDist
    :: SqlBackend -> Trace IO Text -> Text -> Cole.Config
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
            liftIO $ logInfo tracer "Inserting Cole Genesis distribution"
            count <- lift DB.queryBlockCount
            when (count > 0) $
              dbSyncNodeError "insertValidateGenesisDist: Genesis data mismatch."
            void . lift $ DB.insertMeta $
                            DB.Meta
                              { DB.metaStartTime = Cole.configStartTime cfg
                              , DB.metaNetworkName = networkName
                              }

            -- Insert an 'artificial' Genesis block (with a genesis specific slot leader). We
            -- need this block to attach the genesis distribution transactions to.
            -- It would be nice to not need this artificial block, but that would
            -- require plumbing the Genesis.Config into 'insertColeBlockOrEBB'
            -- which would be a pain in the neck.
            slid <- lift . DB.insertSlotLeader $
                            DB.SlotLeader
                              { DB.slotLeaderHash = BS.take 28 $ configGenesisHash cfg
                              , DB.slotLeaderPoolHashId = Nothing
                              , DB.slotLeaderDescription = "Genesis slot leader"
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
                        , DB.blockTime = Cole.configStartTime cfg
                        , DB.blockTxCount = fromIntegral (length $ genesisTxos cfg)
                        -- Genesis block does not have a protocol version, so set this to '0'.
                        , DB.blockProtoMajor = 0
                        , DB.blockProtoMinor = 0
                        -- Sophie specific
                        , DB.blockVrfKey = Nothing
                        , DB.blockOpCert = Nothing
                        , DB.blockOpCertCounter = Nothing
                        }
            lift $ mapM_ (insertTxOuts bid) $ genesisTxos cfg
            liftIO . logInfo tracer $ "Initial genesis distribution populated. Hash "
                            <> renderByteArray (configGenesisHash cfg)

            supply <- lift DB.queryTotalSupply
            liftIO $ logInfo tracer ("Total genesis supply of Bcc: " <> DB.renderBcc supply)

-- | Validate that the initial Genesis distribution in the DB matches the Genesis data.
validateGenesisDistribution
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Text -> Cole.Config -> DB.BlockId
    -> ReaderT SqlBackend m (Either SyncNodeError ())
validateGenesisDistribution tracer networkName cfg bid =
  runExceptT $ do
    meta <- liftLookupFail "validateGenesisDistribution" DB.queryMeta

    when (DB.metaStartTime meta /= Cole.configStartTime cfg) $
      dbSyncNodeError $ Text.concat
            [ "Mismatch chain start time. Config value "
            , textShow (Cole.configStartTime cfg)
            , " does not match DB value of ", textShow (DB.metaStartTime meta)
            ]

    when (DB.metaNetworkName meta /= networkName) $
          dbSyncNodeError $ Text.concat
              [ "validateGenesisDistribution: Provided network name "
              , networkName
              , " does not match DB value "
              , DB.metaNetworkName meta
              ]

    txCount <- lift $ DB.queryBlockTxCount bid
    let expectedTxCount = fromIntegral $length (genesisTxos cfg)
    when (txCount /= expectedTxCount) $
      dbSyncNodeError $ Text.concat
              [ "validateGenesisDistribution: Expected initial block to have "
              , textShow expectedTxCount
              , " but got "
              , textShow txCount
              ]
    totalSupply <- lift DB.queryGenesisSupply
    case DB.word64ToBcc <$> configGenesisSupply cfg of
      Left err -> dbSyncNodeError $ "validateGenesisDistribution: " <> textShow err
      Right expectedSupply ->
        when (expectedSupply /= totalSupply) $
          dbSyncNodeError  $ Text.concat
                [ "validateGenesisDistribution: Expected total supply to be "
                , DB.renderBcc expectedSupply
                , " but got "
                , DB.renderBcc totalSupply
                ]
    liftIO $ do
      logInfo tracer "Initial genesis distribution present and correct"
      logInfo tracer ("Total genesis supply of Bcc: " <> DB.renderBcc totalSupply)

-- -----------------------------------------------------------------------------

insertTxOuts :: (MonadBaseControl IO m, MonadIO m) => DB.BlockId -> (Cole.Address, Cole.Entropic) -> ReaderT SqlBackend m ()
insertTxOuts blkId (address, value) = do
  -- Each address/value pair of the initial coin distribution comes from an artifical transaction
  -- with a hash generated by hashing the address.
  txId <- DB.insertTx $
            DB.Tx
              { DB.txHash = Cole.unTxHash $ txHashOfAddress address
              , DB.txBlockId = blkId
              , DB.txBlockIndex = 0
              , DB.txOutSum = DB.DbEntropic (Cole.unsafeGetEntropic value)
              , DB.txFee = DB.DbEntropic 0
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
              , DB.txOutAddress = Text.decodeUtf8 $ Cole.addrToBase58 address
              , DB.txOutAddressRaw = Binary.serialize' address
              , DB.txOutAddressHasScript = False
              , DB.txOutPaymentCred = Nothing
              , DB.txOutStakeAddressId = Nothing
              , DB.txOutValue = DB.DbEntropic (Cole.unsafeGetEntropic value)
              , DB.txOutDataHash = Nothing
              }

-- -----------------------------------------------------------------------------

configGenesisHash :: Cole.Config -> ByteString
configGenesisHash =
  Cole.unAbstractHash . Cole.unGenesisHash . Cole.configGenesisHash

configGenesisSupply :: Cole.Config -> Either Cole.EntropicError Word64
configGenesisSupply =
  fmap Cole.unsafeGetEntropic . Cole.sumEntropic . map snd . genesisTxos

genesisTxos :: Cole.Config -> [(Cole.Address, Cole.Entropic)]
genesisTxos config =
    avvmBalances <> nonAvvmBalances
  where
    avvmBalances :: [(Cole.Address, Cole.Entropic)]
    avvmBalances =
      first (Cole.makeRedeemAddress networkMagic . Crypto.fromCompactRedeemVerificationKey)
        <$> Map.toList (Cole.unGenesisAvvmBalances $ Cole.configAvvmDistr config)

    networkMagic :: Cole.NetworkMagic
    networkMagic = Cole.makeNetworkMagic (Cole.configProtocolMagic config)

    nonAvvmBalances :: [(Cole.Address, Cole.Entropic)]
    nonAvvmBalances =
      Map.toList $ Cole.unGenesisNonAvvmBalances (Cole.configNonAvvmBalances config)

txHashOfAddress :: Cole.Address -> Crypto.Hash Cole.Tx
txHashOfAddress = coerce . Crypto.serializeCborHash
