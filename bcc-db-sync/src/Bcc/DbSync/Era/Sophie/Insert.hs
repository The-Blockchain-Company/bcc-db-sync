{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Bcc.DbSync.Era.Sophie.Insert
  ( insertSophieBlock
  , postEpochRewards
  , postEpochStake

  , containsUnicodeNul
  , safeDecodeUtf8
  ) where

import           Bcc.Prelude

import           Bcc.Api (SerialiseAsCBOR (..))
import           Bcc.Api.Sophie (TxMetadataValue (..), makeTransactionMetadata,
                   metadataValueToJsonNoSchema)

import           Bcc.BM.Trace (Trace, logDebug, logInfo, logWarning)

import qualified Bcc.Crypto.Hash as Crypto

import           Bcc.Db (DbEntropic (..), DbWord64 (..), SyncState (..))
import qualified Bcc.Db as DB

import           Bcc.DbSync.Era
import qualified Bcc.DbSync.Era.Sophie.Generic as Generic
import           Bcc.DbSync.Era.Sophie.Generic.ParamProposal
import           Bcc.DbSync.Era.Sophie.Insert.Epoch
import           Bcc.DbSync.Era.Sophie.Query
import           Bcc.DbSync.Era.Util (liftLookupFail)

import qualified Bcc.Ledger.Address as Ledger
import qualified Bcc.Ledger.Aurum.Scripts as Ledger
import qualified Bcc.Ledger.BaseTypes as Ledger
import           Bcc.Ledger.Coin (Coin (..))
import qualified Bcc.Ledger.Coin as Ledger
import qualified Bcc.Ledger.Credential as Ledger
import qualified Bcc.Ledger.Keys as Ledger

import           Bcc.Sync.Error
import           Bcc.Sync.LedgerState
import           Bcc.Sync.Types
import           Bcc.Sync.Util

import           Bcc.Ledger.Jen.Value (AssetName (..), PolicyID (..), Value (..))

import           Bcc.Slotting.Block (BlockNo (..))
import           Bcc.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))

import           Control.Monad.Class.MonadSTM.Strict (tryReadTBQueue)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Group (invert)
import qualified Data.Map.Strict as Map
import           Data.Maybe.Strict (strictMaybeToMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

import           Database.Persist.Sql (SqlBackend)

import           Shardagnostic.Consensus.Bcc.Block (StandardCrypto)

import qualified Sophie.Spec.Ledger.PParams as Sophie
import qualified Sophie.Spec.Ledger.STS.Chain as Sophie
import qualified Sophie.Spec.Ledger.TxBody as Sophie

insertSophieBlock
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> LedgerEnv -> Generic.Block -> LedgerStateSnapshot -> SlotDetails
    -> ReaderT SqlBackend m (Either SyncNodeError ())
insertSophieBlock tracer lenv blk lStateSnap details = do
  runExceptT $ do
    pbid <- liftLookupFail (renderInsertName (Generic.blkEra blk)) $ DB.queryBlockId (Generic.blkPreviousHash blk)
    mPhid <- lift $ queryPoolHashId (Generic.blkCreatorPoolHash blk)

    slid <- lift . DB.insertSlotLeader $ Generic.mkSlotLeader (Generic.blkSlotLeader blk) mPhid
    blkId <- lift . DB.insertBlock $
                  DB.Block
                    { DB.blockHash = Generic.blkHash blk
                    , DB.blockEpochNo = Just $ unEpochNo (sdEpochNo details)
                    , DB.blockSlotNo = Just $ unSlotNo (Generic.blkSlotNo blk)
                    , DB.blockEpochSlotNo = Just $ unEpochSlot (sdEpochSlot details)
                    , DB.blockBlockNo = Just $ unBlockNo (Generic.blkBlockNo blk)
                    , DB.blockPreviousId  = Just pbid
                    , DB.blockSlotLeaderId = slid
                    , DB.blockSize = Generic.blkSize blk
                    , DB.blockTime = sdSlotTime details
                    , DB.blockTxCount = fromIntegral $ length (Generic.blkTxs blk)
                    , DB.blockProtoMajor = fromIntegral $ Sophie.pvMajor (Generic.blkProto blk)
                    , DB.blockProtoMinor = fromIntegral $ Sophie.pvMinor (Generic.blkProto blk)

                    -- Sophie specific
                    , DB.blockVrfKey = Just $ Generic.blkVrfKey blk
                    , DB.blockOpCert = Just $ Generic.blkOpCert blk
                    , DB.blockOpCertCounter = Just $ Generic.blkOpCertCounter blk
                    }

    zipWithM_ (insertTx tracer (leNetwork lenv) lStateSnap blkId (sdEpochNo details) (Generic.blkSlotNo blk)) [0 .. ] (Generic.blkTxs blk)

    liftIO $ do
      let epoch = unEpochNo (sdEpochNo details)
          slotWithinEpoch = unEpochSlot (sdEpochSlot details)
          followingClosely = getSyncStatus details == SyncFollowing

      when (followingClosely && slotWithinEpoch /= 0 && unBlockNo (Generic.blkBlockNo blk) `mod` 20 == 0) $ do
        logInfo tracer $
          mconcat
            [ renderInsertName (Generic.blkEra blk), ": continuing epoch ", textShow epoch
            , " (slot ", textShow slotWithinEpoch , "/"
            , textShow (unEpochSize $ sdEpochSize details), ")"
            ]
      logger followingClosely tracer $ mconcat
        [ renderInsertName (Generic.blkEra blk), ": epoch "
        , textShow (unEpochNo $ sdEpochNo details)
        , ", slot ", textShow (unSlotNo $ Generic.blkSlotNo blk)
        , ", block ", textShow (unBlockNo $ Generic.blkBlockNo blk)
        , ", hash ", renderByteArray (Generic.blkHash blk)
        ]

    whenJust (lssNewEpoch lStateSnap) $ \ newEpoch -> do
      insertOnNewEpoch tracer blkId (Generic.blkSlotNo blk) (sdEpochNo details) newEpoch

    mbop <- liftIO . atomically $ tryReadTBQueue (leBulkOpQueue lenv)
    whenJust (maybeToStrict mbop) $ \ bop ->
      insertEpochInterleaved tracer bop

    when (unBlockNo (Generic.blkBlockNo blk) `mod` offlineModBase == 0) .
      lift $ do
        insertOfflineResults tracer (leOfflineResultQueue lenv)
        loadOfflineWorkQueue tracer (leOfflineWorkQueue lenv)

    when (getSyncStatus details == SyncFollowing) $
      -- Serializiing things during syncing can drastically slow down full sync
      -- times (ie 10x or more).
      lift DB.transactionCommit
  where
    logger :: Bool -> Trace IO a -> a -> IO ()
    logger followingClosely
      | followingClosely = logInfo
      | unBlockNo (Generic.blkBlockNo blk) `mod` 5000 == 0 = logInfo
      | otherwise = logDebug

    renderInsertName :: Generic.BlockEra -> Text
    renderInsertName eraName =
      case eraName of
        Generic.Sophie -> "insertSophieBlock"
        other -> mconcat [ "insertSophieBlock(", textShow other, ")" ]

    offlineModBase :: Word64
    offlineModBase =
      case getSyncStatus details of
        SyncFollowing -> 10
        SyncLagging -> 2000

-- -----------------------------------------------------------------------------

insertOnNewEpoch
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.BlockId -> SlotNo -> EpochNo -> Generic.NewEpoch
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertOnNewEpoch tracer blkId slotNo epochNo newEpoch = do
    whenJust (Generic.euProtoParams epochUpdate) $ \ params ->
      insertEpochParam tracer blkId epochNo params (Generic.euNonce epochUpdate)
    whenJust (Generic.neBccPots newEpoch) $ \pots ->
      insertPots blkId slotNo epochNo pots
  where
    epochUpdate :: Generic.EpochUpdate
    epochUpdate = Generic.neEpochUpdate newEpoch

-- -----------------------------------------------------------------------------

insertTx
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Ledger.Network -> LedgerStateSnapshot -> DB.BlockId -> EpochNo -> SlotNo -> Word64 -> Generic.Tx
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertTx tracer network lStateSnap blkId epochNo slotNo blockIndex tx = do
    let fees = unCoin $ Generic.txFees tx
        outSum = unCoin $ Generic.txOutSum tx
        withdrawalSum = unCoin $ Generic.txWithdrawalSum tx
    resolvedInputs <- mapM resolveTxInputs (Generic.txInputs tx)
    let inSum = fromIntegral $ sum $ map (unDbEntropic . thrd3) resolvedInputs
    -- Insert transaction and get txId from the DB.
    txId <- lift . DB.insertTx $
              DB.Tx
                { DB.txHash = Generic.txHash tx
                , DB.txBlockId = blkId
                , DB.txBlockIndex = blockIndex
                , DB.txOutSum = DB.DbEntropic (fromIntegral outSum)
                , DB.txFee = DB.DbEntropic (fromIntegral . unCoin $ Generic.txFees tx)
                , DB.txDeposit = fromIntegral (inSum + withdrawalSum) - fromIntegral (outSum + fees)
                , DB.txSize = Generic.txSize tx
                , DB.txInvalidBefore = DbWord64 . unSlotNo <$> Generic.txInvalidBefore tx
                , DB.txInvalidHereafter = DbWord64 . unSlotNo <$> Generic.txInvalidHereafter tx
                , DB.txValidContract = Generic.txValidContract tx
                , DB.txScriptSize = sum $ Generic.txScriptSizes tx
                }

    -- Insert outputs for a transaction before inputs in case the inputs for this transaction
    -- references the output (not sure this can even happen).
    mapM_ (insertTxOut tracer txId) (Generic.txOutputs tx)

    redeemersIds <- mapM (insertRedeemer tracer txId) (Generic.txRedeemer tx)
    let redeemers = zip redeemersIds (Generic.txRedeemer tx)

    -- Insert the transaction inputs and collateral inputs (Aurum).
    mapM_ (insertTxIn tracer txId redeemers) resolvedInputs
    mapM_ (insertCollateralTxIn tracer txId) (Generic.txCollateralInputs tx)

    case Generic.txMetadata tx of
      Nothing -> pure ()
      Just md -> insertTxMetadata tracer txId md

    mapM_ (insertCertificate tracer lStateSnap network blkId txId epochNo slotNo redeemers) $ Generic.txCertificates tx
    mapM_ (insertWithdrawals tracer txId redeemers) $ Generic.txWithdrawals tx

    mapM_ (insertParamProposal tracer txId) $ Generic.txParamProposal tx

    insertMaTxMint tracer txId $ Generic.txMint tx

    mapM_ (insertScript tracer txId) $ Generic.txScripts tx

resolveTxInputs :: MonadIO m => Generic.TxIn -> ExceptT SyncNodeError (ReaderT SqlBackend m) (Generic.TxIn, DB.TxId, DbEntropic)
resolveTxInputs txIn = do
    res <- liftLookupFail "resolveTxInputs" $ queryResolveInput txIn
    pure $ convert res
  where
    convert :: (DB.TxId, DbEntropic) -> (Generic.TxIn, DB.TxId, DbEntropic)
    convert (txId, entropic) = (txIn, txId, entropic)

insertTxOut
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Generic.TxOut
    -> ExceptT e (ReaderT SqlBackend m) ()
insertTxOut tracer txId (Generic.TxOut index addr value maMap dataHash) = do
    mSaId <- lift $ insertStakeAddressRefIfMissing txId addr
    txOutId <- lift . DB.insertTxOut $
                DB.TxOut
                  { DB.txOutTxId = txId
                  , DB.txOutIndex = index
                  , DB.txOutAddress = Generic.renderAddress addr
                  , DB.txOutAddressRaw = Ledger.serialiseAddr addr
                  , DB.txOutAddressHasScript = hasScript
                  , DB.txOutPaymentCred = Generic.maybePaymentCred addr
                  , DB.txOutStakeAddressId = mSaId
                  , DB.txOutValue = Generic.coinToDbEntropic value
                  , DB.txOutDataHash = dataHash
                  }
    insertMaTxOut tracer txOutId maMap
  where
    hasScript :: Bool
    hasScript = maybe False Generic.hasCredScript (Generic.getPaymentCred addr)

insertTxIn
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> [(DB.RedeemerId, Generic.TxRedeemer)]
    -> (Generic.TxIn, DB.TxId, DbEntropic)
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertTxIn _tracer txInId redeemers (Generic.TxIn _txHash index txInRedeemerIndex, txOutId, _entropic) = do
    void . lift . DB.insertTxIn $
            DB.TxIn
              { DB.txInTxInId = txInId
              , DB.txInTxOutId = txOutId
              , DB.txInTxOutIndex = fromIntegral index
              , DB.txInRedeemerId = fst <$> find redeemerMatches redeemers
              }
  where
    redeemerMatches :: (DB.RedeemerId, Generic.TxRedeemer) -> Bool
    redeemerMatches (_rid, redeemer) =
      Generic.txRedeemerPurpose redeemer == Ledger.Spend
        && Just (Generic.txRedeemerIndex redeemer) == txInRedeemerIndex

insertCollateralTxIn
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Generic.TxIn
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertCollateralTxIn _tracer txInId (Generic.TxIn txId index _) = do
  txOutId <- liftLookupFail "insertCollateralTxIn" $ DB.queryTxId txId
  void . lift . DB.insertCollateralTxIn $
            DB.CollateralTxIn
              { DB.collateralTxInTxInId = txInId
              , DB.collateralTxInTxOutId = txOutId
              , DB.collateralTxInTxOutIndex = fromIntegral index
              }

insertCertificate
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> LedgerStateSnapshot -> Ledger.Network -> DB.BlockId -> DB.TxId -> EpochNo -> SlotNo
    -> [(DB.RedeemerId, Generic.TxRedeemer)]
    -> Generic.TxCertificate
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertCertificate tracer lStateSnap network blkId txId epochNo slotNo redeemers (Generic.TxCertificate ridx idx cert) =
  case cert of
    Sophie.DCertDeleg deleg -> insertDelegCert tracer network txId idx ridx epochNo slotNo redeemers deleg
    Sophie.DCertPool pool -> insertPoolCert tracer lStateSnap network epochNo blkId txId idx pool
    Sophie.DCertMir mir -> insertMirCert tracer network txId idx mir
    Sophie.DCertGenesis _gen -> do
        -- TODO : Low priority
        liftIO $ logWarning tracer "insertCertificate: Unhandled DCertGenesis certificate"
        pure ()


insertPoolCert
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> LedgerStateSnapshot -> Ledger.Network -> EpochNo -> DB.BlockId -> DB.TxId -> Word16 -> Sophie.PoolCert StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolCert tracer lStateSnap network epoch blkId txId idx pCert =
  case pCert of
    Sophie.RegPool pParams -> insertPoolRegister tracer lStateSnap network epoch blkId txId idx pParams
    Sophie.RetirePool keyHash epochNum -> insertPoolRetire txId epochNum idx keyHash

insertDelegCert
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Ledger.Network -> DB.TxId -> Word16 -> Maybe Word64 -> EpochNo -> SlotNo
    -> [(DB.RedeemerId, Generic.TxRedeemer)]
    -> Sophie.DelegCert StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertDelegCert tracer network txId idx ridx epochNo slotNo redeemers dCert =
  case dCert of
    Sophie.RegKey cred -> insertStakeRegistration tracer epochNo txId idx $ Generic.annotateStakingCred network cred
    Sophie.DeRegKey cred -> insertStakeDeregistration tracer network epochNo txId idx ridx redeemers cred
    Sophie.Delegate (Sophie.Delegation cred poolkh) -> insertDelegation tracer network epochNo slotNo txId idx ridx cred redeemers poolkh

insertPoolRegister
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> LedgerStateSnapshot -> Ledger.Network -> EpochNo -> DB.BlockId -> DB.TxId -> Word16 -> Sophie.PoolParams StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolRegister tracer lStateSnap network (EpochNo epoch) blkId txId idx params = do

    when (fromIntegral (Ledger.unCoin $ Sophie._poolPledge params) > maxEntropic) $
      liftIO . logWarning tracer $
        mconcat
          [ "Bad pledge amount: ", textShow (Ledger.unCoin $ Sophie._poolPledge params)
          , " > maxEntropic."
          ]

    when (fromIntegral (Ledger.unCoin $ Sophie._poolCost params) > maxEntropic) $
      liftIO . logWarning tracer $
        mconcat
          [ "Bad fixed cost amount: ", textShow (Ledger.unCoin $ Sophie._poolCost params)
          , " > maxEntropic."
          ]

    poolHashId <- insertPoolHash (Sophie._poolId params)

    mdId <- case strictMaybeToMaybe $ Sophie._poolMD params of
              Just md -> Just <$> insertMetaDataRef poolHashId txId md
              Nothing -> pure Nothing

    epochActivationDelay <- mkEpochActivationDelay poolHashId

    poolUpdateId <- lift . DB.insertPoolUpdate $
                      DB.PoolUpdate
                        { DB.poolUpdateHashId = poolHashId
                        , DB.poolUpdateCertIndex = idx
                        , DB.poolUpdateVrfKeyHash = Crypto.hashToBytes (Sophie._poolVrf params)
                        , DB.poolUpdatePledge = Generic.coinToDbEntropic (Sophie._poolPledge params)
                        , DB.poolUpdateRewardAddr = Generic.serialiseRewardAcntWithNetwork network (Sophie._poolRAcnt params)
                        , DB.poolUpdateActiveEpochNo = epoch + epochActivationDelay
                        , DB.poolUpdateMetaId = mdId
                        , DB.poolUpdateMargin = realToFrac $ Ledger.unboundRational (Sophie._poolMargin params)
                        , DB.poolUpdateFixedCost = Generic.coinToDbEntropic (Sophie._poolCost params)
                        , DB.poolUpdateRegisteredTxId = txId
                        }

    mapM_ (insertPoolOwner network poolHashId txId) $ toList (Sophie._poolOwners params)
    mapM_ (insertPoolRelay poolUpdateId) $ toList (Sophie._poolRelays params)

  where
    mkEpochActivationDelay :: MonadIO m => DB.PoolHashId -> ExceptT SyncNodeError (ReaderT SqlBackend m) Word64
    mkEpochActivationDelay poolHashId =
      if Set.member (Sophie._poolId params) $ getPoolParams (lssOldState lStateSnap)
        then pure 3
        else do
          -- if the pool is not registered at the end of the previous block, check for
          -- other registrations at the current block. If this is the first registration
          -- then it's +2, else it's +3.
          otherUpdates <- lift $ queryPoolUpdateByBlock blkId poolHashId
          pure $ if otherUpdates then 3 else 2


maxEntropic :: Word64
maxEntropic = 45000000000000000

insertPoolHash
    :: forall m . (MonadBaseControl IO m, MonadIO m)
    => Ledger.KeyHash 'Ledger.StakePool StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) DB.PoolHashId
insertPoolHash kh =
    lift . DB.insertPoolHash $
      DB.PoolHash
        { DB.poolHashHashRaw = Generic.unKeyHashRaw kh
        , DB.poolHashView = Generic.unKeyHashView kh
        }


insertPoolRetire
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.TxId -> EpochNo -> Word16 -> Ledger.KeyHash 'Ledger.StakePool StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolRetire txId epochNum idx keyHash = do
  poolId <- liftLookupFail "insertPoolRetire" $ queryStakePoolKeyHash keyHash
  void . lift . DB.insertPoolRetire $
    DB.PoolRetire
      { DB.poolRetireHashId = poolId
      , DB.poolRetireCertIndex = idx
      , DB.poolRetireAnnouncedTxId = txId
      , DB.poolRetireRetiringEpoch = unEpochNo epochNum
      }


insertMetaDataRef
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.PoolHashId -> DB.TxId -> Sophie.PoolMetadata
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) DB.PoolMetadataRefId
insertMetaDataRef poolId txId md =
  lift . DB.insertPoolMetadataRef $
    DB.PoolMetadataRef
      { DB.poolMetadataRefPoolId = poolId
      , DB.poolMetadataRefUrl = Ledger.urlToText (Sophie._poolMDUrl md)
      , DB.poolMetadataRefHash = Sophie._poolMDHash md
      , DB.poolMetadataRefRegisteredTxId = txId
      }

insertStakeAddress
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.TxId -> Sophie.RewardAcnt StandardCrypto
    -> ReaderT SqlBackend m DB.StakeAddressId
insertStakeAddress txId rewardAddr =
  -- If the address already esists in the table, it will not be inserted again (due to
  -- the uniqueness constraint) but the function will return the 'StakeAddressId'.
  DB.insertStakeAddress $
    DB.StakeAddress
      { DB.stakeAddressHashRaw = Ledger.serialiseRewardAcnt rewardAddr
      , DB.stakeAddressView = Generic.renderRewardAcnt rewardAddr
      , DB.stakeAddressScriptHash = Generic.getCredentialScriptHash $ Ledger.getRwdCred rewardAddr
      , DB.stakeAddressRegisteredTxId = txId
      }

insertStakeAddressRefIfMissing
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.TxId -> Ledger.Addr StandardCrypto
    -> ReaderT SqlBackend m (Maybe DB.StakeAddressId)
insertStakeAddressRefIfMissing txId addr =
    maybe insertSAR (pure . Just) =<< queryStakeAddressRef addr
  where
    insertSAR :: (MonadBaseControl IO m, MonadIO m) => ReaderT SqlBackend m (Maybe DB.StakeAddressId)
    insertSAR =
      case addr of
        Ledger.AddrBootstrap {} -> pure Nothing
        Ledger.Addr nw _pcred sref ->
          case sref of
            Ledger.StakeRefBase cred ->
              Just <$> insertStakeAddress txId (Sophie.RewardAcnt nw cred)
            Ledger.StakeRefPtr {} ->
              -- This happens when users pay to payment addresses that refer to a stake addresses
              -- by pointer, but where the pointer does not refer to a registered stake address.
              pure Nothing
            Ledger.StakeRefNull -> pure Nothing

insertPoolOwner
    :: (MonadBaseControl IO m, MonadIO m)
    => Ledger.Network -> DB.PoolHashId -> DB.TxId -> Ledger.KeyHash 'Ledger.Staking StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolOwner network poolHashId txId skh = do
  saId <- lift $ insertStakeAddress txId (Sophie.RewardAcnt network (Ledger.KeyHashObj skh))
  void . lift . DB.insertPoolOwner $
    DB.PoolOwner
      { DB.poolOwnerAddrId = saId
      , DB.poolOwnerPoolHashId = poolHashId
      , DB.poolOwnerRegisteredTxId = txId
      }

insertStakeRegistration
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> EpochNo -> DB.TxId -> Word16 -> Sophie.RewardAcnt StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertStakeRegistration _tracer epochNo txId idx rewardAccount = do
  saId <- lift $ insertStakeAddress txId rewardAccount
  void . lift . DB.insertStakeRegistration $
    DB.StakeRegistration
      { DB.stakeRegistrationAddrId = saId
      , DB.stakeRegistrationCertIndex = idx
      , DB.stakeRegistrationEpochNo = unEpochNo epochNo
      , DB.stakeRegistrationTxId = txId
      }

insertStakeDeregistration
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Ledger.Network -> EpochNo -> DB.TxId -> Word16 -> Maybe Word64
    -> [(DB.RedeemerId, Generic.TxRedeemer)] -> Ledger.StakeCredential StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertStakeDeregistration _tracer network epochNo txId idx ridx redeemers cred = do
    scId <- liftLookupFail "insertStakeDeregistration" $ queryStakeAddress (Generic.stakingCredHash network cred)
    void . lift . DB.insertStakeDeregistration $
      DB.StakeDeregistration
        { DB.stakeDeregistrationAddrId = scId
        , DB.stakeDeregistrationCertIndex = idx
        , DB.stakeDeregistrationEpochNo = unEpochNo epochNo
        , DB.stakeDeregistrationTxId = txId
        , DB.stakeDeregistrationRedeemerId = fst <$> find redeemerMatches redeemers
        }
  where
    redeemerMatches :: (DB.RedeemerId, Generic.TxRedeemer) -> Bool
    redeemerMatches (_rid, redeemer) =
      Generic.txRedeemerPurpose redeemer == Ledger.Cert
        && Just (Generic.txRedeemerIndex redeemer) == ridx

insertDelegation
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Ledger.Network -> EpochNo -> SlotNo -> DB.TxId -> Word16 -> Maybe Word64
    -> Ledger.StakeCredential StandardCrypto
    -> [(DB.RedeemerId, Generic.TxRedeemer)]
    -> Ledger.KeyHash 'Ledger.StakePool StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertDelegation _tracer network (EpochNo epoch) slotNo txId idx ridx cred redeemers poolkh = do
    addrId <- liftLookupFail "insertDelegation" $ queryStakeAddress (Generic.stakingCredHash network cred)
    poolHashId <-liftLookupFail "insertDelegation" $ queryStakePoolKeyHash poolkh
    void . lift . DB.insertDelegation $
      DB.Delegation
        { DB.delegationAddrId = addrId
        , DB.delegationCertIndex = idx
        , DB.delegationPoolHashId = poolHashId
        , DB.delegationActiveEpochNo = epoch + 2 -- The first epoch where this delegation is valid.
        , DB.delegationTxId = txId
        , DB.delegationSlotNo = unSlotNo slotNo
        , DB.delegationRedeemerId = fst <$> find redeemerMatches redeemers
        }
  where
    redeemerMatches :: (DB.RedeemerId, Generic.TxRedeemer) -> Bool
    redeemerMatches (_rid, redeemer) =
      Generic.txRedeemerPurpose redeemer == Ledger.Cert
        && Just (Generic.txRedeemerIndex redeemer) == ridx

insertMirCert
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Ledger.Network -> DB.TxId -> Word16 -> Sophie.MIRCert StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertMirCert _tracer network txId idx mcert = do
    case Sophie.mirPot mcert of
      Sophie.ReservesMIR ->
        case Sophie.mirRewards mcert of
          Sophie.StakeAddressesMIR rwds -> mapM_ insertMirReserves $ Map.toList rwds
          Sophie.SendToOppositePotMIR xfrs -> insertPotTransfer (Ledger.toDeltaCoin xfrs)

      Sophie.TreasuryMIR -> do
        case Sophie.mirRewards mcert of
          Sophie.StakeAddressesMIR rwds -> mapM_ insertMirTreasury $ Map.toList rwds
          Sophie.SendToOppositePotMIR xfrs -> insertPotTransfer (invert $ Ledger.toDeltaCoin xfrs)

  where
    insertMirReserves
        :: (MonadBaseControl IO m, MonadIO m)
        => (Ledger.StakeCredential StandardCrypto, Ledger.DeltaCoin)
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insertMirReserves (cred, dcoin) = do
      addrId <- lift . insertStakeAddress txId $ Generic.annotateStakingCred network cred
      void . lift . DB.insertReserve $
        DB.Reserve
          { DB.reserveAddrId = addrId
          , DB.reserveCertIndex = idx
          , DB.reserveTxId = txId
          , DB.reserveAmount = DB.deltaCoinToDbInt65 dcoin
          }

    insertMirTreasury
        :: (MonadBaseControl IO m, MonadIO m)
        => (Ledger.StakeCredential StandardCrypto, Ledger.DeltaCoin)
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insertMirTreasury (cred, dcoin) = do
      addrId <- lift . insertStakeAddress txId $ Generic.annotateStakingCred network cred
      void . lift . DB.insertTreasury $
        DB.Treasury
          { DB.treasuryAddrId = addrId
          , DB.treasuryCertIndex = idx
          , DB.treasuryTxId = txId
          , DB.treasuryAmount = DB.deltaCoinToDbInt65 dcoin
          }

    insertPotTransfer
        :: (MonadBaseControl IO m, MonadIO m)
        => Ledger.DeltaCoin -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insertPotTransfer dcoinTreasury =
      void . lift . DB.insertPotTransfer $
        DB.PotTransfer
          { DB.potTransferCertIndex = idx
          , DB.potTransferTreasury = DB.deltaCoinToDbInt65 dcoinTreasury
          , DB.potTransferReserves = DB.deltaCoinToDbInt65 (invert dcoinTreasury)
          , DB.potTransferTxId = txId
          }

insertWithdrawals
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> [(DB.RedeemerId, Generic.TxRedeemer)]
    -> Generic.TxWithdrawal
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertWithdrawals _tracer txId redeemers (Generic.TxWithdrawal index account coin) = do
    addrId <- liftLookupFail "insertWithdrawals" $ queryStakeAddress (Ledger.serialiseRewardAcnt account)
    void . lift . DB.insertWithdrawal $
      DB.Withdrawal
        { DB.withdrawalAddrId = addrId
        , DB.withdrawalTxId = txId
        , DB.withdrawalAmount = Generic.coinToDbEntropic coin
        , DB.withdrawalRedeemerId = fst <$> find redeemerMatches redeemers
        }
  where
    redeemerMatches :: (DB.RedeemerId, Generic.TxRedeemer) -> Bool
    redeemerMatches (_rid, redeemer) =
      Generic.txRedeemerPurpose redeemer == Ledger.Rewrd &&
      Just (Generic.txRedeemerIndex redeemer) == index

insertPoolRelay
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.PoolUpdateId -> Sophie.StakePoolRelay
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolRelay updateId relay =
  void . lift . DB.insertPoolRelay $
    case relay of
      Sophie.SingleHostAddr mPort mIpv4 mIpv6 ->
        DB.PoolRelay -- An IPv4 and/or IPv6 address
          { DB.poolRelayUpdateId = updateId
          , DB.poolRelayIpv4 = textShow <$> strictMaybeToMaybe mIpv4
          , DB.poolRelayIpv6 = textShow <$> strictMaybeToMaybe mIpv6
          , DB.poolRelayDnsName = Nothing
          , DB.poolRelayDnsSrvName = Nothing
          , DB.poolRelayPort = Ledger.portToWord16 <$> strictMaybeToMaybe mPort
          }
      Sophie.SingleHostName mPort name ->
        DB.PoolRelay -- An A or AAAA DNS record
          { DB.poolRelayUpdateId = updateId
          , DB.poolRelayIpv4 = Nothing
          , DB.poolRelayIpv6 = Nothing
          , DB.poolRelayDnsName = Just (Ledger.dnsToText name)
          , DB.poolRelayDnsSrvName = Nothing
          , DB.poolRelayPort = Ledger.portToWord16 <$> strictMaybeToMaybe mPort
          }
      Sophie.MultiHostName name ->
        DB.PoolRelay -- An SRV DNS record
          { DB.poolRelayUpdateId = updateId
          , DB.poolRelayIpv4 = Nothing
          , DB.poolRelayIpv6 = Nothing
          , DB.poolRelayDnsName = Nothing
          , DB.poolRelayDnsSrvName = Just (Ledger.dnsToText name)
          , DB.poolRelayPort = Nothing
          }

insertParamProposal
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> ParamProposal
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertParamProposal _tracer txId pp =
  void . lift . DB.insertParamProposal $
    DB.ParamProposal
      { DB.paramProposalRegisteredTxId = txId

      , DB.paramProposalEpochNo = unEpochNo $ pppEpochNo pp
      , DB.paramProposalKey = pppKey pp
      , DB.paramProposalMinFeeA = fromIntegral <$> pppMinFeeA pp
      , DB.paramProposalMinFeeB = fromIntegral <$> pppMinFeeB pp
      , DB.paramProposalMaxBlockSize = fromIntegral <$> pppMaxBhSize pp
      , DB.paramProposalMaxTxSize = fromIntegral <$> pppMaxTxSize pp
      , DB.paramProposalMaxBhSize = fromIntegral <$> pppMaxBhSize pp
      , DB.paramProposalKeyDeposit = Generic.coinToDbEntropic <$> pppKeyDeposit pp
      , DB.paramProposalPoolDeposit = Generic.coinToDbEntropic <$> pppPoolDeposit pp
      , DB.paramProposalMaxEpoch = unEpochNo <$> pppMaxEpoch pp
      , DB.paramProposalOptimalPoolCount = fromIntegral <$> pppOptimalPoolCount pp
      , DB.paramProposalInfluence = fromRational <$> pppInfluence pp
      , DB.paramProposalMonetaryExpandRate = Generic.unitIntervalToDouble <$> pppMonetaryExpandRate pp
      , DB.paramProposalTreasuryGrowthRate = Generic.unitIntervalToDouble <$> pppTreasuryGrowthRate pp
      , DB.paramProposalDecentralisation = Generic.unitIntervalToDouble <$> pppDecentralisation pp
      , DB.paramProposalEntropy = Generic.nonceToBytes =<< pppEntropy pp
      , DB.paramProposalProtocolMajor = fromIntegral . Sophie.pvMajor <$> pppProtocolVersion pp
      , DB.paramProposalProtocolMinor = fromIntegral . Sophie.pvMinor <$> pppProtocolVersion pp
      , DB.paramProposalMinUtxoValue = Generic.coinToDbEntropic <$> pppMinUtxoValue pp
      , DB.paramProposalMinPoolCost = Generic.coinToDbEntropic <$> pppMinPoolCost pp

      -- New for Aurum

      , DB.paramProposalCoinsPerUtxoWord = Generic.coinToDbEntropic <$> pppCoinsPerUtxoWord pp
      , DB.paramProposalCostModels = Generic.renderLanguageCostModel <$> pppCostmdls pp
      , DB.paramProposalPriceMem = realToFrac <$> pppPriceMem pp
      , DB.paramProposalPriceStep = realToFrac <$> pppPriceStep pp
      , DB.paramProposalMaxTxExMem = DbWord64 <$> pppMaxTxExMem pp
      , DB.paramProposalMaxTxExSteps = DbWord64 <$> pppMaxTxExSteps pp
      , DB.paramProposalMaxBlockExMem = DbWord64 <$> pppMaxBlockExMem pp
      , DB.paramProposalMaxBlockExSteps = DbWord64 <$> pppMaxBlockExSteps pp
      , DB.paramProposalMaxValSize = DbWord64 . fromIntegral <$> pppMaxValSize pp
      , DB.paramProposalCollateralPercent = fromIntegral <$> pppCollateralPercentage pp
      , DB.paramProposalMaxCollateralInputs = fromIntegral <$> pppMaxCollateralInputs pp
      }

insertRedeemer
  :: (MonadBaseControl IO m, MonadIO m)
  => Trace IO Text -> DB.TxId -> Generic.TxRedeemer
  -> ExceptT SyncNodeError (ReaderT SqlBackend m) DB.RedeemerId
insertRedeemer _tr txId redeemer = do
    scriptHash <- findScriptHash
    lift . DB.insertRedeemer $
      DB.Redeemer
        { DB.redeemerTxId = txId
        , DB.redeemerUnitMem = Generic.txRedeemerMem redeemer
        , DB.redeemerUnitSteps = Generic.txRedeemerSteps redeemer
        , DB.redeemerFee = DB.DbEntropic (fromIntegral . unCoin $ Generic.txRedeemerFee redeemer)
        , DB.redeemerPurpose = mkPurpose $ Generic.txRedeemerPurpose redeemer
        , DB.redeemerIndex = Generic.txRedeemerIndex redeemer
        , DB.redeemerScriptHash = scriptHash
        }
  where
    mkPurpose :: Ledger.Tag -> DB.ScriptPurpose
    mkPurpose tag =
      case tag of
        Ledger.Spend -> DB.Spend
        Ledger.Mint -> DB.Mint
        Ledger.Cert -> DB.Cert
        Ledger.Rewrd -> DB.Rewrd

    findScriptHash
      :: (MonadBaseControl IO m, MonadIO m)
      => ExceptT SyncNodeError (ReaderT SqlBackend m) (Maybe ByteString)
    findScriptHash =
      case Generic.txRedeemerScriptHash redeemer of
        Nothing -> pure Nothing
        Just (Right bs) -> pure $ Just bs
        Just (Left txIn) -> fst <$> liftLookupFail "insertRedeemer" (queryResolveInputCredentials txIn)

insertTxMetadata
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Map Word64 TxMetadataValue
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertTxMetadata tracer txId metadata =
    mapM_ insert $ Map.toList metadata
  where
    insert
        :: (MonadBaseControl IO m, MonadIO m)
        => (Word64, TxMetadataValue)
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insert (key, md) = do
      let jsonbs = LBS.toStrict $ Aeson.encode (metadataValueToJsonNoSchema md)
          singleKeyCBORMetadata = serialiseToCBOR $ makeTransactionMetadata (Map.singleton key md)
      ejson <- liftIO $ safeDecodeUtf8 jsonbs
      mjson <- case ejson of
                 Left err -> do
                   liftIO . logWarning tracer $ mconcat
                      [ "insertTxMetadata: Could not decode to UTF8: ", textShow err ]
                   pure Nothing
                 Right json ->
                   -- See https://github.com/The-Blockchain-Company/bcc-db-sync/issues/297
                   if containsUnicodeNul json
                     then do
                       liftIO $ logWarning tracer "insertTxMetadata: dropped due to a Unicode NUL character."
                       pure Nothing
                     else
                       pure $ Just json
      void . lift . DB.insertTxMetadata $
        DB.TxMetadata
          { DB.txMetadataKey = DbWord64 key
          , DB.txMetadataJson = mjson
          , DB.txMetadataBytes = singleKeyCBORMetadata
          , DB.txMetadataTxId = txId
          }

safeDecodeUtf8 :: ByteString -> IO (Either Text.UnicodeException Text)
safeDecodeUtf8 bs
    | BS.any isNullChar bs = pure $ Left (Text.DecodeError (BS.unpack bs) (Just 0))
    | otherwise = try $ evaluate (Text.decodeUtf8With Text.strictDecode bs)
  where
    isNullChar :: Char -> Bool
    isNullChar ch = ord ch == 0

containsUnicodeNul :: Text -> Bool
containsUnicodeNul = Text.isInfixOf "\\u000"

insertEpochParam
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.BlockId -> EpochNo -> Generic.ProtoParams -> Ledger.Nonce
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertEpochParam _tracer blkId (EpochNo epoch) params nonce =
  void . lift . DB.insertEpochParam $
    DB.EpochParam
      { DB.epochParamEpochNo = epoch
      , DB.epochParamMinFeeA = fromIntegral (Generic.ppMinfeeA params)
      , DB.epochParamMinFeeB = fromIntegral (Generic.ppMinfeeB params)
      , DB.epochParamMaxBlockSize = fromIntegral (Generic.ppMaxBBSize params)
      , DB.epochParamMaxTxSize = fromIntegral (Generic.ppMaxTxSize params)
      , DB.epochParamMaxBhSize = fromIntegral (Generic.ppMaxBHSize params)
      , DB.epochParamKeyDeposit = Generic.coinToDbEntropic (Generic.ppKeyDeposit params)
      , DB.epochParamPoolDeposit = Generic.coinToDbEntropic (Generic.ppPoolDeposit params)
      , DB.epochParamMaxEpoch = unEpochNo (Generic.ppMaxEpoch params)
      , DB.epochParamOptimalPoolCount = fromIntegral (Generic.ppOptialPoolCount params)
      , DB.epochParamInfluence = fromRational (Generic.ppInfluence params)
      , DB.epochParamMonetaryExpandRate = Generic.unitIntervalToDouble (Generic.ppMonetaryExpandRate params)
      , DB.epochParamTreasuryGrowthRate = Generic.unitIntervalToDouble (Generic.ppTreasuryGrowthRate params)
      , DB.epochParamDecentralisation = Generic.unitIntervalToDouble (Generic.ppDecentralisation params)
      , DB.epochParamEntropy = Generic.nonceToBytes $ Generic.ppExtraEntropy params
      , DB.epochParamProtocolMajor = fromIntegral $ Sophie.pvMajor (Generic.ppProtocolVersion params)
      , DB.epochParamProtocolMinor = fromIntegral $ Sophie.pvMinor (Generic.ppProtocolVersion params)
      , DB.epochParamMinUtxoValue = Generic.coinToDbEntropic (Generic.ppMinUTxOValue params)
      , DB.epochParamMinPoolCost = Generic.coinToDbEntropic (Generic.ppMinPoolCost params)
      , DB.epochParamNonce = Generic.nonceToBytes nonce
      , DB.epochParamCoinsPerUtxoWord = Generic.coinToDbEntropic <$> Generic.ppCoinsPerUtxoWord params
      , DB.epochParamCostModels = Generic.renderLanguageCostModel <$> Generic.ppCostmdls params
      , DB.epochParamPriceMem = realToFrac <$> Generic.ppPriceMem params
      , DB.epochParamPriceStep = realToFrac <$> Generic.ppPriceStep params
      , DB.epochParamMaxTxExMem = DbWord64 <$> Generic.ppMaxTxExMem params
      , DB.epochParamMaxTxExSteps = DbWord64 <$> Generic.ppMaxTxExSteps params
      , DB.epochParamMaxBlockExMem = DbWord64 <$> Generic.ppMaxBlockExMem params
      , DB.epochParamMaxBlockExSteps = DbWord64 <$> Generic.ppMaxBlockExSteps params
      , DB.epochParamMaxValSize = DbWord64 . fromIntegral <$> Generic.ppMaxValSize params
      , DB.epochParamCollateralPercent = fromIntegral <$> Generic.ppCollateralPercentage params
      , DB.epochParamMaxCollateralInputs = fromIntegral <$> Generic.ppMaxCollateralInputs params
      , DB.epochParamBlockId = blkId
      }

insertMaTxMint
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Value StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertMaTxMint _tracer txId (Value _adaShouldAlwaysBeZeroButWeDoNotCheck mintMap) =
    mapM_ insertOuter $ Map.toList mintMap
  where
    insertOuter
        :: (MonadBaseControl IO m, MonadIO m)
        => (PolicyID StandardCrypto, Map AssetName Integer)
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insertOuter (policy, aMap) =
      mapM_ (insertInner policy) $ Map.toList aMap

    insertInner
        :: (MonadBaseControl IO m, MonadIO m)
        => PolicyID StandardCrypto -> (AssetName, Integer)
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insertInner policy (aname, amount) =
      void . lift . DB.insertMaTxMint $
        DB.MaTxMint
          { DB.maTxMintPolicy = Generic.unScriptHash (policyID policy)
          , DB.maTxMintName = assetName aname
          , DB.maTxMintQuantity = DB.integerToDbInt65 amount
          , DB.maTxMintTxId = txId
          }

insertMaTxOut
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxOutId -> Map (PolicyID StandardCrypto) (Map AssetName Integer)
    -> ExceptT e (ReaderT SqlBackend m) ()
insertMaTxOut _tracer txOutId maMap =
    mapM_ insertOuter $ Map.toList maMap
  where
    insertOuter
        :: (MonadBaseControl IO m, MonadIO m)
        => (PolicyID StandardCrypto, Map AssetName Integer)
        -> ExceptT e (ReaderT SqlBackend m) ()
    insertOuter (policy, aMap) =
      mapM_ (insertInner policy) $ Map.toList aMap

    insertInner
        :: (MonadBaseControl IO m, MonadIO m)
        => PolicyID StandardCrypto -> (AssetName, Integer)
        -> ExceptT e (ReaderT SqlBackend m) ()
    insertInner policy (aname, amount) =
      void . lift . DB.insertMaTxOut $
        DB.MaTxOut
          { DB.maTxOutPolicy = Generic.unScriptHash (policyID policy)
          , DB.maTxOutName = assetName aname
          , DB.maTxOutQuantity = DbWord64 (fromIntegral amount)
          , DB.maTxOutTxOutId = txOutId
          }

insertScript
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Generic.TxScript
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertScript _tracer txId script = do
    void . lift . DB.insertScript $
      DB.Script
        { DB.scriptTxId = txId
        , DB.scriptHash = Generic.txScriptHash script
        , DB.scriptType = scriptType
        , DB.scriptSerialisedSize = Generic.txScriptZerepochSize script
        }
  where
    scriptType :: DB.ScriptType
    scriptType =
      case Generic.txScriptZerepochSize script of
        Nothing -> DB.Timelock
        Just _ -> DB.Zerepoch

insertPots
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.BlockId
    -> SlotNo -> EpochNo
    -> Sophie.BccPots
    -> ExceptT e (ReaderT SqlBackend m) ()
insertPots blockId slotNo epochNo pots =
    void . lift $ DB.insertBccPots $
      DB.BccPots
        { DB.adaPotsSlotNo = unSlotNo slotNo
        , DB.adaPotsEpochNo = unEpochNo epochNo
        , DB.adaPotsTreasury = Generic.coinToDbEntropic $ Sophie.treasuryBccPot pots
        , DB.adaPotsReserves = Generic.coinToDbEntropic $ Sophie.reservesBccPot pots
        , DB.adaPotsRewards = Generic.coinToDbEntropic $ Sophie.rewardsBccPot pots
        , DB.adaPotsUtxo = Generic.coinToDbEntropic $ Sophie.utxoBccPot pots
        , DB.adaPotsDeposits = Generic.coinToDbEntropic $ Sophie.depositsBccPot pots
        , DB.adaPotsFees = Generic.coinToDbEntropic $ Sophie.feesBccPot pots
        , DB.adaPotsBlockId = blockId
        }
