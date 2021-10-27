{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Godx.DbSync.Era.Cole.Insert
  ( insertColeBlock
  ) where

import           Godx.Prelude

import           Godx.BM.Trace (Trace, logDebug, logInfo)
import           Godx.Binary (serialize')

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import qualified Godx.Binary as Binary

-- Import all 'bcc-ledger' functions and data types qualified so they do not
-- clash with the Godx.Db functions and data types which are also imported
-- qualified.
import qualified Godx.Chain.Block as Cole hiding (blockHash)
import qualified Godx.Chain.Common as Cole
import qualified Godx.Chain.UTxO as Cole
import qualified Godx.Chain.Update as Cole hiding (protocolVersion)

import qualified Godx.Crypto as Crypto (serializeCborHash)

import           Godx.Db (DbIsaac (..), SyncState (..))
import           Godx.DbSync.Era.Util (liftLookupFail)

import           Godx.Sync.Types

import           Godx.Slotting.Slot (EpochNo (..), EpochSize (..))

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Database.Persist.Sql (SqlBackend)

import qualified Godx.Db as DB
import qualified Godx.DbSync.Era.Cole.Util as Cole
import           Godx.Sync.Error
import           Godx.Sync.Util

import           Shardagnostic.Consensus.Cole.Ledger (ColeBlock (..))

-- Trivial local data type for use in place of a tuple.
data ValueFee = ValueFee
  { vfValue :: !DbIsaac
  , vfFee :: !DbIsaac
  }

insertColeBlock
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> ColeBlock -> SlotDetails
    -> ReaderT SqlBackend m (Either SyncNodeError ())
insertColeBlock tracer blk details = do
  res <- runExceptT $
            case coleBlockRaw blk of
              Cole.ABOBBlock ablk -> insertABlock tracer ablk details
              Cole.ABOBBoundary abblk -> insertABOBBoundary tracer abblk details
  -- Serializiing things during syncing can drastically slow down full sync
  -- times (ie 10x or more).
  when (getSyncStatus details == SyncFollowing)
    DB.transactionCommit
  pure res


insertABOBBoundary
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Cole.ABoundaryBlock ByteString -> SlotDetails
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertABOBBoundary tracer blk details = do
  -- Will not get called in the OBFT part of the Cole era.
  let prevHash = case Cole.boundaryPrevHash (Cole.boundaryHeader blk) of
                    Left gh -> Cole.genesisToHeaderHash gh
                    Right hh -> hh
  pbid <- liftLookupFail "insertABOBBoundary" $ DB.queryBlockId (Cole.unHeaderHash prevHash)
  slid <- lift . DB.insertSlotLeader $
                  DB.SlotLeader
                    { DB.slotLeaderHash = BS.replicate 28 '\0'
                    , DB.slotLeaderPoolHashId = Nothing
                    , DB.slotLeaderDescription = "Epoch boundary slot leader"
                    }
  void . lift . DB.insertBlock $
            DB.Block
              { DB.blockHash = Cole.unHeaderHash $ Cole.boundaryHashAnnotated blk
              , DB.blockEpochNo = Just $ unEpochNo (sdEpochNo details)
              -- No slotNo for a boundary block
              , DB.blockSlotNo = Nothing
              , DB.blockEpochSlotNo = Nothing
              , DB.blockBlockNo = Nothing
              , DB.blockPreviousId = Just pbid
              , DB.blockSlotLeaderId = slid
              , DB.blockSize = fromIntegral $ Cole.boundaryBlockLength blk
              , DB.blockTime = sdSlotTime details
              , DB.blockTxCount = 0
              -- EBBs do not seem to have protocol version fields, so set this to '0'.
              , DB.blockProtoMajor = 0
              , DB.blockProtoMinor = 0
              -- Sophie specific
              , DB.blockVrfKey = Nothing
              , DB.blockOpCert = Nothing
              , DB.blockOpCertCounter = Nothing
              }

  liftIO . logInfo tracer $
        Text.concat
          [ "insertABOBBoundary: epoch "
          , textShow (Cole.boundaryEpoch $ Cole.boundaryHeader blk)
          , ", hash "
          , Cole.renderAbstractHash (Cole.boundaryHashAnnotated blk)
          ]

insertABlock
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Cole.ABlock ByteString -> SlotDetails
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertABlock tracer blk details = do
    pbid <- liftLookupFail "insertABlock" $ DB.queryBlockId (Cole.unHeaderHash $ Cole.blockPreviousHash blk)
    slid <- lift . DB.insertSlotLeader $ Cole.mkSlotLeader blk
    blkId <- lift . DB.insertBlock $
                  DB.Block
                    { DB.blockHash = Cole.blockHash blk
                    , DB.blockEpochNo = Just $ unEpochNo (sdEpochNo details)
                    , DB.blockSlotNo = Just $ Cole.slotNumber blk
                    , DB.blockEpochSlotNo = Just $ unEpochSlot (sdEpochSlot details)
                    , DB.blockBlockNo = Just $ Cole.blockNumber blk
                    , DB.blockPreviousId = Just pbid
                    , DB.blockSlotLeaderId = slid
                    , DB.blockSize = fromIntegral $ Cole.blockLength blk
                    , DB.blockTime = sdSlotTime details
                    , DB.blockTxCount = fromIntegral $ length (Cole.blockPayload blk)
                    , DB.blockProtoMajor = Cole.pvMajor (Cole.protocolVersion blk)
                    , DB.blockProtoMinor = Cole.pvMinor (Cole.protocolVersion blk)
                    -- Sophie specific
                    , DB.blockVrfKey = Nothing
                    , DB.blockOpCert = Nothing
                    , DB.blockOpCertCounter = Nothing
                    }

    zipWithM_ (insertTx tracer blkId) (Cole.blockPayload blk) [ 0 .. ]

    liftIO $ do
      let epoch = unEpochNo (sdEpochNo details)
          slotWithinEpoch = unEpochSlot (sdEpochSlot details)
          followingClosely = getSyncStatus details == SyncFollowing

      when (followingClosely && slotWithinEpoch /= 0 && Cole.blockNumber blk `mod` 20 == 0) $ do
        logInfo tracer $
          mconcat
            [ "insertColeBlock: continuing epoch ", textShow epoch
            , " (slot ", textShow slotWithinEpoch , "/"
            , textShow (unEpochSize $ sdEpochSize details), ")"
            ]
      logger followingClosely tracer $ mconcat
        [ "insertColeBlock: epoch ", textShow (unEpochNo $ sdEpochNo details)
        , ", slot ", textShow (Cole.slotNumber blk)
        , ", block ", textShow (Cole.blockNumber blk)
        , ", hash ", renderByteArray (Cole.blockHash blk)
        ]
  where
    logger :: Bool -> Trace IO a -> a -> IO ()
    logger followingClosely
      | followingClosely = logInfo
      | Cole.blockNumber blk `mod` 5000 == 0 = logInfo
      | otherwise = logDebug


insertTx
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.BlockId -> Cole.TxAux -> Word64
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertTx tracer blkId tx blockIndex = do
    resolvedInputs <- mapM resolveTxInputs (toList $ Cole.txInputs (Cole.taTx tx))
    valFee <- firstExceptT annotateTx $ ExceptT $ pure (calculateTxFee (Cole.taTx tx) resolvedInputs)
    txId <- lift . DB.insertTx $
              DB.Tx
                { DB.txHash = Cole.unTxHash $ Crypto.serializeCborHash (Cole.taTx tx)
                , DB.txBlockId = blkId
                , DB.txBlockIndex = blockIndex
                , DB.txOutSum = vfValue valFee
                , DB.txFee = vfFee valFee
                , DB.txDeposit = 0 -- Cole does not have deposits/refunds
                -- Would be really nice to have a way to get the transaction size
                -- without re-serializing it.
                , DB.txSize = fromIntegral $ BS.length (serialize' $ Cole.taTx tx)
                , DB.txInvalidHereafter = Nothing
                , DB.txInvalidBefore = Nothing
                , DB.txValidContract = True
                , DB.txScriptSize = 0
                }

    -- Insert outputs for a transaction before inputs in case the inputs for this transaction
    -- references the output (not sure this can even happen).
    lift $ zipWithM_ (insertTxOut tracer txId) [0 ..] (toList . Cole.txOutputs $ Cole.taTx tx)
    mapMVExceptT (insertTxIn tracer txId) resolvedInputs
  where
    annotateTx :: SyncNodeError -> SyncNodeError
    annotateTx ee =
      case ee of
        NEInvariant loc ei -> NEInvariant loc (annotateInvariantTx (Cole.taTx tx) ei)
        _other -> ee

insertTxOut
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Word32 -> Cole.TxOut
    -> ReaderT SqlBackend m ()
insertTxOut _tracer txId index txout =
  void . DB.insertTxOut $
            DB.TxOut
              { DB.txOutTxId = txId
              , DB.txOutIndex = fromIntegral index
              , DB.txOutAddress = Text.decodeUtf8 $ Cole.addrToBase58 (Cole.txOutAddress txout)
              , DB.txOutAddressRaw = Binary.serialize' (Cole.txOutAddress txout)
              , DB.txOutAddressHasScript = False
              , DB.txOutPaymentCred = Nothing -- Cole does not have a payment credential.
              , DB.txOutStakeAddressId = Nothing -- Cole does not have a stake address.
              , DB.txOutValue = DbIsaac (Cole.unsafeGetIsaac $ Cole.txOutValue txout)
              , DB.txOutDataHash = Nothing
              }


insertTxIn
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> (Cole.TxIn, DB.TxId, DbIsaac)
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertTxIn _tracer txInId (Cole.TxInUtxo _txHash inIndex, txOutId, _isaac) = do
  void . lift . DB.insertTxIn $
            DB.TxIn
              { DB.txInTxInId = txInId
              , DB.txInTxOutId = txOutId
              , DB.txInTxOutIndex = fromIntegral inIndex
              , DB.txInRedeemerId = Nothing
              }

-- -----------------------------------------------------------------------------

resolveTxInputs :: MonadIO m => Cole.TxIn -> ExceptT SyncNodeError (ReaderT SqlBackend m) (Cole.TxIn, DB.TxId, DbIsaac)
resolveTxInputs txIn@(Cole.TxInUtxo txHash index) = do
    res <- liftLookupFail "resolveInput" $ DB.queryTxOutValue (Cole.unTxHash txHash, fromIntegral index)
    pure $ convert res
  where
    convert :: (DB.TxId, DbIsaac) -> (Cole.TxIn, DB.TxId, DbIsaac)
    convert (txId, isaac) = (txIn, txId, isaac)

calculateTxFee :: Cole.Tx -> [(Cole.TxIn, DB.TxId, DbIsaac)] -> Either SyncNodeError ValueFee
calculateTxFee tx resolvedInputs = do
      outval <- first (\e -> NEError $ "calculateTxFee: " <> textShow e) output
      when (null resolvedInputs) $
        Left $ NEError "calculateTxFee: List of transaction inputs is zero."
      let inval = sum $ map (unDbIsaac . thrd3) resolvedInputs
      if inval < outval
        then Left $ NEInvariant "calculateTxFee" $ EInvInOut inval outval
        else Right $ ValueFee (DbIsaac outval) (DbIsaac $ inval - outval)
  where
    output :: Either Cole.IsaacError Word64
    output =
      Cole.unsafeGetIsaac
        <$> Cole.sumIsaac (map Cole.txOutValue $ Cole.txOutputs tx)

-- | An 'ExceptT' version of 'mapM_' which will 'left' the first 'Left' it finds.
mapMVExceptT :: Monad m => (a -> ExceptT e m ()) -> [a] -> ExceptT e m ()
mapMVExceptT action xs =
  case xs of
    [] -> pure ()
    (y:ys) -> action y >> mapMVExceptT action ys
