{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Bcc.DbSync.Plugin.Default
  ( defDbSyncNodePlugin
  , insertDefaultBlock
  , rollbackToPoint
  ) where


import           Bcc.Prelude

import           Bcc.BM.Trace (Trace, logDebug, logInfo)

import qualified Bcc.Db as DB

import           Bcc.DbSync.Era

import           Bcc.DbSync.Era.Cole.Insert (insertColeBlock)
import           Bcc.DbSync.Era.Bcc.Insert (insertEpochSyncTime)
import           Bcc.DbSync.Era.Sophie.Adjust (adjustEpochRewards)
import qualified Bcc.DbSync.Era.Sophie.Generic as Generic
import           Bcc.DbSync.Era.Sophie.Insert (insertSophieBlock)
import           Bcc.DbSync.Era.Sophie.Insert.Epoch
import           Bcc.DbSync.Era.Sophie.Validate
import           Bcc.DbSync.Rollback (rollbackToPoint)

import           Bcc.Ledger.Coin (Coin (..))
import           Bcc.Ledger.Credential (StakeCredential)
import           Bcc.Ledger.Crypto (StandardCrypto)

import           Bcc.Slotting.Slot (EpochNo (..))

import           Bcc.Sync.Api
import           Bcc.Sync.Error
import           Bcc.Sync.LedgerState
import           Bcc.Sync.Plugin
import           Bcc.Sync.Types
import           Bcc.Sync.Util

import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Class.MonadSTM.Strict (putTMVar, tryTakeTMVar)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (newExceptT)

import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map

import           Database.Persist.Sql (SqlBackend)

import           Shardagnostic.Consensus.Bcc.Block (HardForkBlock (..))

import           System.IO.Unsafe (unsafePerformIO)

-- | The default SyncNodePlugin.
-- Does exactly what the bcc-db-sync node did before the plugin system was added.
-- The non-default node takes this structure and extends the lists.
defDbSyncNodePlugin :: SqlBackend -> SyncNodePlugin
defDbSyncNodePlugin backend =
  SyncNodePlugin
    { plugOnStartup = []
    , plugInsertBlock = [insertDefaultBlock backend]
    , plugRollbackBlock = [rollbackToPoint backend]
    }

-- -------------------------------------------------------------------------------------------------

insertDefaultBlock
    :: SqlBackend -> Trace IO Text -> SyncEnv -> [BlockDetails]
    -> IO (Either SyncNodeError ())
insertDefaultBlock backend tracer env blockDetails = do
    thisIsAnUglyHack tracer (envLedger env)
    DB.runDbBcccoinLogging backend tracer $
      runExceptT (traverse_ insert blockDetails)
  where
    insert
        :: (MonadBaseControl IO m, MonadCatch m, MonadIO m)
        => BlockDetails -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insert (BlockDetails cblk details) = do
      -- Calculate the new ledger state to pass to the DB insert functions but do not yet
      -- update ledgerStateVar.
      let lenv = envLedger env
      lStateSnap <- liftIO $ applyBlock (envLedger env) cblk details
      mkSnapshotMaybe lStateSnap (isSyncedWithinSeconds details 60)
      handleLedgerEvents tracer (envLedger env) (lssPoint lStateSnap) (lssEvents lStateSnap)
      case cblk of
        BlockCole blk ->
          newExceptT $ insertColeBlock tracer blk details
        BlockSophie blk ->
          newExceptT $ insertSophieBlock tracer lenv (Generic.fromSophieBlock blk) lStateSnap details
        BlockAllegra blk ->
          newExceptT $ insertSophieBlock tracer lenv (Generic.fromAllegraBlock blk) lStateSnap details
        BlockJen blk ->
          newExceptT $ insertSophieBlock tracer lenv (Generic.fromJenBlock blk) lStateSnap details
        BlockAurum blk -> do
          let pp = getAurumPParams $ lssState lStateSnap
          newExceptT $ insertSophieBlock tracer lenv (Generic.fromAurumBlock pp blk) lStateSnap details

    mkSnapshotMaybe
        :: (MonadBaseControl IO m, MonadIO m)
        => LedgerStateSnapshot -> DB.SyncState
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    mkSnapshotMaybe snapshot syncState =
      whenJust (lssNewEpoch snapshot) $ \newEpoch -> do
        liftIO $ logDebug (leTrace $ envLedger env) "Preparing for a snapshot"
        let newEpochNo = Generic.neEpoch newEpoch
        -- flush all volatile data
        finalizeEpochBulkOps (envLedger env)
        liftIO $ logDebug (leTrace $ envLedger env) "Taking a ledger a snapshot"
        -- finally take a ledger snapshot
        -- TODO: Instead of newEpochNo - 1, is there any way to get the epochNo from 'lssOldState'?
        liftIO $ saveCleanupState (envLedger env) (lssOldState snapshot) syncState  (Just $ newEpochNo - 1)

-- -------------------------------------------------------------------------------------------------
-- This horrible hack is only need because of the split between `bcc-sync` and `bcc-db-sync`.

{-# NOINLINE offlineThreadStarted #-}
offlineThreadStarted :: IORef Bool
offlineThreadStarted = unsafePerformIO $ newIORef False

thisIsAnUglyHack :: Trace IO Text -> LedgerEnv -> IO ()
thisIsAnUglyHack tracer lenv = do
  started <- readIORef offlineThreadStarted
  unless started $ do
    -- This is horrible!
    writeIORef offlineThreadStarted True
    void . async $ runOfflineFetchThread tracer lenv
    logInfo tracer "thisIsAnUglyHack: Main thead"

-- -------------------------------------------------------------------------------------------------

handleLedgerEvents
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> LedgerEnv -> BccPoint -> [LedgerEvent]
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
handleLedgerEvents tracer lenv point =
    mapM_ handler
  where
    handler
        :: (MonadBaseControl IO m, MonadIO m)
        => LedgerEvent -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    handler ev =
      case ev of
        LedgerNewEpoch en ss -> do
          lift $ do
            insertEpochSyncTime en ss (leEpochSyncTime lenv)
            adjustEpochRewards tracer (en - 2)
          finalizeEpochBulkOps lenv
          -- Commit everything in the db *AFTER* the epoch rewards have been inserted, the orphaned
          -- rewards removed and the bulk operations finalized.
          lift DB.transactionCommit
          liftIO . logInfo tracer $ "Starting epoch " <> textShow (unEpochNo en)
        LedgerStartAtEpoch en ->
          -- This is different from the previous case in that the db-sync started
          -- in this epoch, for example after a restart, instead of after an epoch boundary.
          liftIO . logInfo tracer $ "Starting at epoch " <> textShow (unEpochNo en)
        LedgerRewards _details rwds -> do
          liftIO . logInfo tracer $ mconcat
            [ "Handling ", show (Map.size (Generic.rwdRewards rwds)), " rewards for epoch "
            , show (unEpochNo $ Generic.rwdEpoch rwds), " ", renderPoint point
            ]
          postEpochRewards lenv rwds point
        LedgerStakeDist sdist -> do
          liftIO . logInfo tracer $ mconcat
            [ "Handling ", show (Map.size (Generic.sdistStakeMap sdist)), " stakes for epoch "
            , show (unEpochNo $ Generic.sdistEpochNo sdist), " ", renderPoint point
            ]
          postEpochStake lenv sdist point
        LedgerRewardDist en rd ->
          lift $ stashPoolRewards tracer lenv en rd
        LedgerMirDist md ->
          lift $ stashMirRewards tracer lenv md

-- These two functions must handle being called in either order.
stashPoolRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> LedgerEnv -> EpochNo -> Map (StakeCredential StandardCrypto) Coin
    -> ReaderT SqlBackend m ()
stashPoolRewards tracer lenv epoch rmap = do
  mMirRwd <- liftIO . atomically $ tryTakeTMVar (leMirRewards lenv)
  case mMirRwd of
    Nothing ->
      liftIO . atomically $ putTMVar (lePoolRewards lenv) (epoch, rmap)
    Just mirMap ->
      validateEpochRewards tracer (leNetwork lenv) (epoch - 2) (Map.unionWith plusCoin rmap mirMap)

stashMirRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> LedgerEnv -> Map (StakeCredential StandardCrypto) Coin
    -> ReaderT SqlBackend m ()
stashMirRewards tracer lenv mirMap = do
    mRwds <- liftIO . atomically $ tryTakeTMVar (lePoolRewards lenv)
    case mRwds of
      Nothing ->
        liftIO . atomically $ putTMVar (leMirRewards lenv) mirMap
      Just (epoch, rmap) ->
        validateEpochRewards tracer (leNetwork lenv) (epoch - 2) (Map.unionWith plusCoin rmap mirMap)
