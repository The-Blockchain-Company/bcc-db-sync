{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Godx.Sync.LedgerState
  ( BulkOperation (..)
  , GodxLedgerState (..)
  , IndexCache (..)
  , LedgerEnv (..)
  , LedgerEvent (..)
  , LedgerStateSnapshot (..)
  , LedgerStateFile (..)
  , mkLedgerEnv
  , applyBlock
  , saveCleanupState
  , listLedgerStateFilesOrdered
  , loadLedgerStateFromFile
  , writeLedgerState
  , findStateFromPoint
  , findLedgerStateFile
  , loadLedgerAtPoint
  , hashToAnnotation
  , getHeaderHash
  , ledgerTipBlockNo
  , getPoolParams
  , getAurumPParams
  ) where

import           Prelude (String, id)

import           Godx.BM.Trace (Trace, logInfo, logWarning)

import           Godx.Binary (DecoderError)
import qualified Godx.Binary as Serialize

import           Godx.Db (SyncState (..))
import qualified Godx.Db as DB

import qualified Godx.Ledger.BaseTypes as Ledger
import           Godx.Ledger.Coin (Coin)
import           Godx.Ledger.Core (PParams)
import           Godx.Ledger.Credential (StakeCredential)
import           Godx.Ledger.Era
import           Godx.Ledger.Keys (KeyHash (..), KeyRole (..))
import           Godx.Ledger.Sophie.Constraints (UsesValue)

import           Godx.Sync.Config.Types
import qualified Godx.Sync.Era.Godx.Util as Godx
import           Godx.Sync.Era.Sophie.Generic (StakeCred)
import qualified Godx.Sync.Era.Sophie.Generic as Generic
import           Godx.Sync.LedgerEvent
import           Godx.Sync.Types hiding (GodxBlock)
import           Godx.Sync.Util

import           Godx.Prelude hiding (atomically)
import           Godx.Slotting.Block (BlockNo (..))

import           Godx.Slotting.EpochInfo (EpochInfo, epochInfoEpoch)
import           Godx.Slotting.Slot (EpochNo (..), SlotNo (..), WithOrigin (..), fromWithOrigin)

import qualified Control.Exception as Exception
import           Control.Monad.Class.MonadSTM.Strict (StrictTMVar, StrictTVar, TBQueue, atomically,
                   flushTBQueue, newEmptyTMVarIO, newTBQueueIO, newTVarIO, readTVar, writeTBQueue,
                   writeTVar)

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as BSS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime, getCurrentTime)

import           Shardagnostic.Consensus.Block (CodecConfig, Point (..), WithOrigin (..), blockHash,
                   blockIsEBB, blockPoint, blockPrevHash, pointSlot)
import           Shardagnostic.Consensus.Block.Abstract (ConvertRawHash (..))
import           Shardagnostic.Consensus.Godx.Block (LedgerState (..), StandardAurum,
                   StandardCrypto)
import           Shardagnostic.Consensus.Godx.CanHardFork ()
import           Shardagnostic.Consensus.Config (TopLevelConfig (..), configCodec, configLedger)
import qualified Shardagnostic.Consensus.HardFork.Combinator as Consensus
import           Shardagnostic.Consensus.HardFork.Combinator.Basics (LedgerState (..))
import           Shardagnostic.Consensus.HardFork.Combinator.State (epochInfoLedger)
import qualified Shardagnostic.Consensus.HeaderValidation as Consensus
import           Shardagnostic.Consensus.Ledger.Abstract (LedgerResult (..), getTipSlot, ledgerTipHash,
                   ledgerTipPoint, ledgerTipSlot, tickThenReapplyLedgerResult)
import           Shardagnostic.Consensus.Ledger.Extended (ExtLedgerCfg (..), ExtLedgerState (..))
import qualified Shardagnostic.Consensus.Ledger.Extended as Consensus
import qualified Shardagnostic.Consensus.Node.ProtocolInfo as Consensus
import           Shardagnostic.Consensus.Sophie.Ledger.Block
import qualified Shardagnostic.Consensus.Sophie.Ledger.Ledger as Consensus
import           Shardagnostic.Consensus.Storage.Serialisation (DecodeDisk (..), EncodeDisk (..))

import           Shardagnostic.Network.AnchoredSeq (Anchorable (..), AnchoredSeq (..))
import qualified Shardagnostic.Network.AnchoredSeq as AS
import           Shardagnostic.Network.Block (HeaderHash, Point (..))
import qualified Shardagnostic.Network.Point as Point

import           Sophie.Spec.Ledger.LedgerState (EpochState (..))
import qualified Sophie.Spec.Ledger.LedgerState as Sophie
import qualified Sophie.Spec.Ledger.STS.Chain as Sophie

import           System.Directory (doesFileExist, listDirectory, removeFile)
import           System.FilePath (dropExtension, takeExtension, (</>))
import           System.Mem (performMajorGC)


-- Note: The decision on whether a ledger-state is written to disk is based on the block number
-- rather than the slot number because while the block number is fully populated (for every block
-- other then genesis with number N there exists a block with number N - 1) whereas in the Sophie
-- era, only about 1/20 slots are occupied with blocks.
-- However, rollbacks are specified using a Point (basically a tuple of SlotNo and hash) and
-- therefore ledger states are stored in files with the SlotNo and hash in the file name.

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use readTVarIO" -}

-- 'GodxPoint' indicates at which point the 'BulkOperation' became available.
-- It is only used in case of a rollback.
data BulkOperation
  = BulkRewardChunk !EpochNo !GodxPoint !IndexCache ![(StakeCred, Set Generic.Reward)]
  | BulkRewardReport !EpochNo !GodxPoint !Int !Coin
  | BulkStakeDistChunk !EpochNo !GodxPoint !IndexCache ![(StakeCred, (Coin, PoolKeyHash))]
  | BulkStakeDistReport !EpochNo !GodxPoint !Int

getBulkOpPoint :: BulkOperation -> GodxPoint
getBulkOpPoint = go
  where
    go (BulkRewardChunk _ point _ _) = point
    go (BulkRewardReport _ point _ _) = point
    go (BulkStakeDistChunk _ point _ _) = point
    go (BulkStakeDistReport _ point _) = point

data IndexCache = IndexCache
  { icAddressCache :: !(Map Generic.StakeCred DB.StakeAddressId)
  , icPoolCache :: !(Map PoolKeyHash DB.PoolHashId)
  }

data LedgerEnv = LedgerEnv
  { leTrace :: Trace IO Text
  , leProtocolInfo :: !(Consensus.ProtocolInfo IO GodxBlock)
  , leDir :: !LedgerStateDir
  , leNetwork :: !Ledger.Network
  , leStateVar :: !(StrictTVar IO (Maybe LedgerDB))
  , leEventState :: !(StrictTVar IO LedgerEventState)
  , lePoolRewards :: !(StrictTMVar IO (EpochNo, Map (StakeCredential StandardCrypto) Coin))
  , leMirRewards :: !(StrictTMVar IO (Map (StakeCredential StandardCrypto) Coin))
  -- The following do not really have anything to do with maintaining ledger
  -- state. They are here due to the ongoing headaches around the split between
  -- `bcc-sync` and `bcc-db-sync`.
  , leIndexCache :: !(StrictTVar IO IndexCache)
  , leBulkOpQueue :: !(TBQueue IO BulkOperation)
  , leOfflineWorkQueue :: !(TBQueue IO PoolFetchRetry)
  , leOfflineResultQueue :: !(TBQueue IO FetchResult)
  , leEpochSyncTime :: !(StrictTVar IO UTCTime)
  , leStableEpochSlot :: !EpochSlot
  }

data LedgerEventState = LedgerEventState
  { lesInitialized :: !Bool
  , lesEpochNo :: !(Maybe EpochNo)
  , lesLastRewardsEpoch :: !(Maybe EpochNo)
  , lesLastStateDistEpoch :: !(Maybe EpochNo)
  , lesLastAdded :: !GodxPoint
  }

topLevelConfig :: LedgerEnv -> TopLevelConfig GodxBlock
topLevelConfig = Consensus.pInfoConfig . leProtocolInfo

newtype GodxLedgerState = GodxLedgerState
  { clsState :: ExtLedgerState GodxBlock
  }

data LedgerStateFile = LedgerStateFile
  { lsfSlotNo :: !SlotNo
  , lsfHash :: !ByteString
  , lsNewEpoch :: !(Maybe EpochNo)
  , lsfFilePath :: !FilePath
  } deriving Show

data LedgerStateSnapshot = LedgerStateSnapshot
  { lssState :: !GodxLedgerState
  , lssOldState :: !GodxLedgerState
  , lssNewEpoch :: !(Strict.Maybe Generic.NewEpoch) -- Only Just for a single block at the epoch boundary
  , lssSlotDetails :: !SlotDetails
  , lssPoint :: !GodxPoint
  , lssEvents :: ![LedgerEvent]
  }

newtype LedgerDB = LedgerDB
  { ledgerDbCheckpoints :: AnchoredSeq (WithOrigin SlotNo) GodxLedgerState GodxLedgerState
  }

pushLedgerDB :: LedgerDB -> GodxLedgerState -> LedgerDB
pushLedgerDB db st =
  pruneLedgerDb 10 db
    { ledgerDbCheckpoints = ledgerDbCheckpoints db :> st
    }

-- | Prune snapshots until at we have at most @k@ snapshots in the LedgerDB,
-- excluding the snapshots stored at the anchor.
pruneLedgerDb :: Word64 -> LedgerDB -> LedgerDB
pruneLedgerDb k db =
  db { ledgerDbCheckpoints = AS.anchorNewest k (ledgerDbCheckpoints db) }

instance Anchorable (WithOrigin SlotNo) GodxLedgerState GodxLedgerState where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot . clsState

-- | The ledger state at the tip of the chain
ledgerDbCurrent :: LedgerDB -> GodxLedgerState
ledgerDbCurrent = either id id . AS.head . ledgerDbCheckpoints

mkLedgerEnv
    :: Trace IO Text -> Consensus.ProtocolInfo IO GodxBlock -> LedgerStateDir
    -> Ledger.Network -> EpochSlot
    -> IO LedgerEnv
mkLedgerEnv trce protocolInfo dir nw stableEpochSlot = do
    svar <- newTVarIO Nothing
    evar <- newTVarIO initLedgerEventState
    ivar <- newTVarIO $ IndexCache mempty mempty
    -- 2.5 days worth of slots. If we try to stick more than this number of
    -- items in the queue, bad things are likely to happen.
    boq <- newTBQueueIO 10800
    owq <- newTBQueueIO 100
    orq <- newTBQueueIO 100
    est <- newTVarIO =<< getCurrentTime
    prvar <- newEmptyTMVarIO
    mrvar <- newEmptyTMVarIO
    pure LedgerEnv
      { leTrace = trce
      , leProtocolInfo = protocolInfo
      , leDir = dir
      , leNetwork = nw
      , leStateVar = svar
      , leEventState = evar
      , lePoolRewards = prvar
      , leMirRewards = mrvar
      , leIndexCache = ivar
      , leBulkOpQueue = boq
      , leOfflineWorkQueue = owq
      , leOfflineResultQueue  = orq
      , leEpochSyncTime = est
      , leStableEpochSlot = stableEpochSlot
      }
  where
    initLedgerEventState :: LedgerEventState
    initLedgerEventState =
      LedgerEventState
        { lesInitialized = False
        , lesEpochNo = Nothing
        , lesLastRewardsEpoch = Nothing
        , lesLastStateDistEpoch = Nothing
        , lesLastAdded = GenesisPoint
        }


initGodxLedgerState :: Consensus.ProtocolInfo IO GodxBlock -> GodxLedgerState
initGodxLedgerState pInfo = GodxLedgerState
      { clsState = Consensus.pInfoInitLedger pInfo
      }

-- TODO make this type safe. We make the assumption here that the first message of
-- the chainsync protocol is 'RollbackTo'.
readStateUnsafe :: LedgerEnv -> STM LedgerDB
readStateUnsafe env = do
    mState <- readTVar $ leStateVar env
    case mState of
      Nothing -> panic "LedgerState.readStateUnsafe: Ledger state is not found"
      Just st -> pure st

-- The function 'tickThenReapply' does zero validation, so add minimal validation ('blockPrevHash'
-- matches the tip hash of the 'LedgerState'). This was originally for debugging but the check is
-- cheap enough to keep.
applyBlock :: LedgerEnv -> GodxBlock -> SlotDetails -> IO LedgerStateSnapshot
applyBlock env blk details =
    -- 'LedgerStateVar' is just being used as a mutable variable. There should not ever
    -- be any contention on this variable, so putting everything inside 'atomically'
    -- is fine.
    atomically $ do
      ledgerDB <- readStateUnsafe env
      let oldState = ledgerDbCurrent ledgerDB
      let !result = applyBlk (ExtLedgerCfg (topLevelConfig env)) blk (clsState oldState)
      let !newState = oldState { clsState = lrResult result }
      let !ledgerDB' = pushLedgerDB ledgerDB newState
      writeTVar (leStateVar env) (Just ledgerDB')
      oldEventState <- readTVar (leEventState env)
      events <- generateEvents env oldEventState  details newState (blockPoint blk)
      pure $ LedgerStateSnapshot
                { lssState = newState
                , lssOldState = oldState
                , lssNewEpoch = maybeToStrict $ mkNewEpoch oldState newState
                , lssSlotDetails = details
                , lssPoint = blockPoint blk
                , lssEvents = events ++ mapMaybe convertAuxLedgerEvent (lrEvents result)
                }
  where
    applyBlk
        :: ExtLedgerCfg GodxBlock -> GodxBlock
        -> ExtLedgerState GodxBlock
        -> LedgerResult (ExtLedgerState GodxBlock) (ExtLedgerState GodxBlock)
    applyBlk cfg block lsb =
      case tickThenReapplyCheckHash cfg block lsb of
        Left err -> panic err
        Right result -> result

    mkNewEpoch :: GodxLedgerState -> GodxLedgerState -> Maybe Generic.NewEpoch
    mkNewEpoch oldState newState =
      if ledgerEpochNo env newState /= ledgerEpochNo env oldState + 1
        then Nothing
        else
          Just $
            Generic.NewEpoch
              { Generic.neEpoch = ledgerEpochNo env newState
              , Generic.neIsEBB = isJust $ blockIsEBB blk
              , Generic.neGodxPots = maybeToStrict $ getGodxPots newState
              , Generic.neEpochUpdate = Generic.epochUpdate (clsState newState)
              }

generateEvents :: LedgerEnv -> LedgerEventState -> SlotDetails -> GodxLedgerState -> GodxPoint -> STM [LedgerEvent]
generateEvents env oldEventState details cls pnt = do
    writeTVar (leEventState env) newEventState
    pure $ catMaybes
            [ newEpochEvent
            , LedgerRewards details <$> rewards
            , LedgerStakeDist <$> stakeDist
            ]
  where
    currentEpochNo :: EpochNo
    currentEpochNo = sdEpochNo details

    newEpochEvent :: Maybe LedgerEvent
    newEpochEvent =
      case lesEpochNo oldEventState of
        Nothing -> Just $ LedgerStartAtEpoch currentEpochNo
        Just oldEpoch ->
          if currentEpochNo == 1 + oldEpoch
            then Just $ LedgerNewEpoch currentEpochNo (getSyncStatus details)
            else Nothing

    -- Want the rewards event to be delivered once only, on a single slot.
    rewards :: Maybe Generic.Rewards
    rewards =
      case lesLastRewardsEpoch oldEventState of
        Nothing -> mkRewards
        Just oldRewardEpoch ->
          if sdEpochSlot details >= leStableEpochSlot env && oldRewardEpoch < currentEpochNo
            then mkRewards
            else Nothing


    mkRewards :: Maybe Generic.Rewards
    mkRewards = Generic.epochRewards (leNetwork env) (sdEpochNo details) (clsState cls)

    stakeDist :: Maybe Generic.StakeDist
    stakeDist =
      case lesLastStateDistEpoch oldEventState of
        Nothing -> mkStakeDist
        Just oldStakeEpoch ->
          if oldStakeEpoch < currentEpochNo
            then mkStakeDist
            else Nothing

    mkStakeDist :: Maybe Generic.StakeDist
    mkStakeDist = Generic.epochStakeDist (leNetwork env) (sdEpochNo details) (clsState cls)

    newEventState :: LedgerEventState
    newEventState =
      LedgerEventState
        { lesInitialized = True
        , lesEpochNo = Just currentEpochNo
        , lesLastRewardsEpoch =
            if isJust rewards
              then Just currentEpochNo
              else lesLastRewardsEpoch oldEventState
        , lesLastStateDistEpoch =
            if isJust stakeDist
              then Just currentEpochNo
              else lesLastStateDistEpoch oldEventState
        , lesLastAdded =
            if isNothing rewards && isNothing stakeDist
              then lesLastAdded oldEventState
              else pnt
        }

saveCurrentLedgerState :: LedgerEnv -> ExtLedgerState GodxBlock -> Maybe EpochNo -> IO ()
saveCurrentLedgerState env ledger mEpochNo = do
    case mkLedgerStateFilename (leDir env) ledger mEpochNo of
      Origin -> pure () -- we don't store genesis
      At file -> do
        exists <- doesFileExist file
        if exists then
          logInfo (leTrace env) $ mconcat
            ["File ", Text.pack file, " exists"]
        else do
          LBS.writeFile file $
            Serialize.serializeEncoding $
              Consensus.encodeExtLedgerState
                 (encodeDisk codecConfig)
                 (encodeDisk codecConfig)
                 (encodeDisk codecConfig)
                 ledger
          logInfo (leTrace env) $ mconcat ["Took a ledger snapshot at ", Text.pack file]
  where
    codecConfig :: CodecConfig GodxBlock
    codecConfig = configCodec (topLevelConfig env)

mkLedgerStateFilename :: LedgerStateDir -> ExtLedgerState GodxBlock -> Maybe EpochNo -> WithOrigin FilePath
mkLedgerStateFilename dir ledger mEpochNo = lsfFilePath . dbPointToFileName dir mEpochNo
    <$> getPoint (ledgerTipPoint (Proxy @GodxBlock) (ledgerState ledger))

saveCleanupState :: LedgerEnv -> GodxLedgerState -> SyncState -> Maybe EpochNo -> IO ()
saveCleanupState env ledger _syncState mEpochNo = do
  let st = clsState ledger
  saveCurrentLedgerState env st mEpochNo
  cleanupLedgerStateFiles env $
    fromWithOrigin (SlotNo 0) (ledgerTipSlot $ ledgerState st)

hashToAnnotation :: ByteString -> ByteString
hashToAnnotation = Base16.encode . BS.take 5

mkRawHash :: HeaderHash GodxBlock -> ByteString
mkRawHash = toRawHash (Proxy @GodxBlock)

mkShortHash :: HeaderHash GodxBlock -> ByteString
mkShortHash = hashToAnnotation . mkRawHash

dbPointToFileName :: LedgerStateDir -> Maybe EpochNo -> Point.Block SlotNo (HeaderHash GodxBlock) -> LedgerStateFile
dbPointToFileName (LedgerStateDir stateDir) mEpochNo (Point.Block slot hash) =
    LedgerStateFile
      { lsfSlotNo = slot
      , lsfHash = shortHash
      , lsNewEpoch = mEpochNo
      , lsfFilePath =
          mconcat
            [ stateDir </> show (unSlotNo slot)
            , "-"
            , BS.unpack shortHash
            , epochSuffix
            , ".lstate"
            ]
      }
  where
    shortHash :: ByteString
    shortHash = mkShortHash hash

    epochSuffix :: String
    epochSuffix =
      case mEpochNo of
        Nothing -> ""
        Just epoch -> "-" ++ show (unEpochNo epoch)

parseLedgerStateFileName :: LedgerStateDir -> FilePath -> Maybe LedgerStateFile
parseLedgerStateFileName (LedgerStateDir stateDir) fp =
    case break (== '-') (dropExtension fp) of
      (slotStr, '-': hashEpoch) -> do
        slot <- readMaybe slotStr
        case break (== '-') hashEpoch of
          (hash, '-' : suffix) | Just epochNo <- readMaybe suffix -> do
            Just $ build (BS.pack hash) slot (Just epochNo)
          (hash, "") ->
            Just $ build (BS.pack hash) slot Nothing
          _otherwise -> Nothing
      _otherwise -> Nothing
  where
    build :: ByteString -> Word64 -> Maybe Word64 -> LedgerStateFile
    build hash slot mEpochNo =
      LedgerStateFile
        { lsfSlotNo = SlotNo slot
        , lsfHash = hash
        , lsNewEpoch = EpochNo <$> mEpochNo
        , lsfFilePath = stateDir </> fp
        }

-- -------------------------------------------------------------------------------------------------

cleanupLedgerStateFiles :: LedgerEnv -> SlotNo -> IO ()
cleanupLedgerStateFiles env slotNo = do
    files <- listLedgerStateFilesOrdered (leDir env)
    let (epochBoundary, valid, invalid) = foldr groupFiles ([], [], []) files
    -- Remove invalid (ie SlotNo >= current) ledger state files (occurs on rollback).
    deleteAndLogFiles env "invalid" invalid
    -- Remove all but 8 most recent state files.
    deleteAndLogStateFile env "valid" (List.drop 8 valid)
    -- Remove all but 2 most recent epoch boundary state files.
    deleteAndLogStateFile env "epoch boundary" (List.drop 2 epochBoundary)
  where
    groupFiles :: LedgerStateFile
               -> ([LedgerStateFile], [LedgerStateFile], [FilePath])
               -> ([LedgerStateFile], [LedgerStateFile], [FilePath]) -- (epochBoundary, valid, invalid)
    groupFiles lFile (epochBoundary, regularFile, invalid)
      | lsfSlotNo lFile > slotNo =
        (epochBoundary, regularFile, lsfFilePath lFile : invalid)
      | Just _ <- lsNewEpoch lFile =
        (lFile : epochBoundary, regularFile, invalid)
      | otherwise =
        (epochBoundary, lFile : regularFile, invalid)

loadLedgerAtPoint :: LedgerEnv -> GodxPoint -> IO (Either [LedgerStateFile] GodxLedgerState)
loadLedgerAtPoint env point = do
    mLedgerDB <- atomically $ readTVar $ leStateVar env
    -- First try to find the ledger in memory
    let mAnchoredSeq = rollbackLedger mLedgerDB
    case mAnchoredSeq of
      Nothing -> do
        -- Ledger states are growing to become very big in memory.
        -- Before parsing the new ledger state we need to make sure the old states
        -- are or can be garbage collected.
        writeLedgerState env Nothing
        performMajorGC
        mst <- findStateFromPoint env point
        case mst of
          Right st -> do
            writeLedgerState env (Just . LedgerDB $ AS.Empty st)
            logInfo (leTrace env) $ mconcat [ "Found snapshot file for ", renderPoint point ]
            pure $ Right st
          Left lsfs -> pure $ Left lsfs
      Just anchoredSeq' -> do
        logInfo (leTrace env) $ mconcat ["Found in memory ledger snapshot at ", renderPoint point ]
        let ledgerDB' = LedgerDB anchoredSeq'
        let st = ledgerDbCurrent ledgerDB'
        eventSt <- atomically $ readTVar $ leEventState env
        when (point < lesLastAdded eventSt) $
          -- This indicates there are at least some BulkOperation after the point
          drainBulkOperation env point
        deleteNewerFiles env point
        writeLedgerState env $ Just ledgerDB'
        pure $ Right st
  where
    rollbackLedger
        :: Maybe LedgerDB
        -> Maybe (AnchoredSeq (WithOrigin SlotNo) GodxLedgerState GodxLedgerState)
    rollbackLedger mLedgerDB = do
      ledgerDB <- mLedgerDB
      AS.rollback (pointSlot point) (const True) (ledgerDbCheckpoints ledgerDB)

-- Filter out the BulkOperation's added after the specific point.
drainBulkOperation :: LedgerEnv -> GodxPoint -> IO ()
drainBulkOperation lenv point = do
    bops <- atomically $ flushTBQueue (leBulkOpQueue lenv)
    let bops' = filter (\bop -> getBulkOpPoint bop <= point) bops
    let removed = length bops - length bops'
    unless (removed == 0) $
      logInfo (leTrace lenv) $ mconcat
        ["Removing ", show removed, " BulkOperations added after ", show point]
    atomically $ mapM_ (writeTBQueue (leBulkOpQueue lenv)) bops'
    pure ()

deleteNewerFiles :: LedgerEnv -> GodxPoint -> IO ()
deleteNewerFiles env point = do
  files <- listLedgerStateFilesOrdered (leDir env)
    -- Genesis can be reproduced from configuration.
    -- TODO: We can make this a monadic action (reread config from disk) to save some memory.
  case getPoint point of
    Origin -> do
      deleteAndLogStateFile env "newer" files
    At blk -> do
      let (newerFiles, _found, _olderFiles) =
            findLedgerStateFile files (Point.blockPointSlot blk, mkRawHash $ Point.blockPointHash blk)
      deleteAndLogStateFile env "newer" newerFiles

deleteAndLogFiles :: LedgerEnv -> Text -> [FilePath] -> IO ()
deleteAndLogFiles env descr files = unless (null files) $ do
  logInfo (leTrace env) $ mconcat ["Removing ", descr, " files ", textShow files]
  mapM_ safeRemoveFile files

deleteAndLogStateFile :: LedgerEnv -> Text -> [LedgerStateFile] -> IO ()
deleteAndLogStateFile env descr lsfs = deleteAndLogFiles env descr (lsfFilePath <$> lsfs)

findStateFromPoint :: LedgerEnv -> GodxPoint -> IO (Either [LedgerStateFile] GodxLedgerState)
findStateFromPoint env point = do
  files <- listLedgerStateFilesOrdered (leDir env)
    -- Genesis can be reproduced from configuration.
    -- TODO: We can make this a monadic action (reread config from disk) to save some memory.
  case getPoint point of
    Origin -> do
      deleteAndLogStateFile env "newer" files
      pure . Right $ initGodxLedgerState (leProtocolInfo env)
    At blk -> do
      let (newerFiles, found, olderFiles) =
            findLedgerStateFile files (Point.blockPointSlot blk, mkRawHash $ Point.blockPointHash blk)
      deleteAndLogStateFile env "newer" newerFiles
      case found of
        Just lsf -> do
          mState <- loadLedgerStateFromFile (topLevelConfig env) False lsf
          case mState of
            Left err -> do
              deleteLedgerFile err lsf
              logNewerFiles olderFiles
              pure $ Left olderFiles
            Right st -> pure $ Right st
        Nothing -> do
          logNewerFiles olderFiles
          pure $ Left olderFiles
  where
    deleteLedgerFile :: Text -> LedgerStateFile -> IO ()
    deleteLedgerFile err lsf = do
      logWarning (leTrace env) $ mconcat
        [ "Failed to parse ledger state file ", Text.pack (lsfFilePath  lsf)
        , " with error '", err, "'. Deleting it."
        ]
      safeRemoveFile $ lsfFilePath lsf

    logNewerFiles :: [LedgerStateFile] -> IO ()
    logNewerFiles lsfs =
      logWarning (leTrace env) $
        case lsfs of
          [] -> "Rollback failed. No more ledger state files."
          (x:_) -> mconcat [ "Rolling back further to slot ", textShow (unSlotNo $ lsfSlotNo x) ]

-- Splits the files based on the comparison with the given point. It will return
-- a list of newer files, a file at the given point if found and a list of older
-- files. All lists of files should be ordered most recent first.
--
-- Newer files can be deleted
-- File at the exact point can be used to initial the LedgerState
-- Older files can be used to rollback even further.
--
-- Files with same slot, but different hash are considered newer.
findLedgerStateFile
    :: [LedgerStateFile] -> (SlotNo, ByteString)
    -> ([LedgerStateFile], Maybe LedgerStateFile, [LedgerStateFile])
findLedgerStateFile files pointPair =
        go [] files
      where
        go newerFiles [] = (reverse newerFiles, Nothing, [])
        go newerFiles (file : rest) =
          case comparePointToFile file pointPair of
            EQ -> (reverse newerFiles, Just file, rest) -- found the file we were looking for
            LT -> (reverse newerFiles, Nothing, file : rest) -- found an older file first
            GT -> go (file : newerFiles) rest -- keep looking on older files

comparePointToFile :: LedgerStateFile -> (SlotNo, ByteString) -> Ordering
comparePointToFile lsf (blSlotNo, blHash) =
  case compare (lsfSlotNo lsf) blSlotNo of
    EQ ->
      if hashToAnnotation blHash == lsfHash lsf
        then EQ
        else GT
    x -> x

loadLedgerStateFromFile :: TopLevelConfig GodxBlock -> Bool -> LedgerStateFile -> IO (Either Text GodxLedgerState)
loadLedgerStateFromFile config delete lsf = do
    mst <- safeReadFile (lsfFilePath lsf)
    case mst of
      Left err -> when delete (safeRemoveFile $ lsfFilePath lsf) >> pure (Left err)
      Right st -> pure . Right $ GodxLedgerState { clsState = st }
  where
    safeReadFile :: FilePath -> IO (Either Text (ExtLedgerState GodxBlock))
    safeReadFile fp = do
      mbs <- Exception.try $ BS.readFile fp
      case mbs of
        Left (err :: IOException) -> pure $ Left (Text.pack $ displayException err)
        Right bs ->
          case decode bs of
            Left err -> pure $ Left $ textShow err
            Right ls -> pure $ Right ls

    codecConfig :: CodecConfig GodxBlock
    codecConfig = configCodec config

    decode :: ByteString -> Either DecoderError (ExtLedgerState GodxBlock)
    decode =
      Serialize.decodeFullDecoder
          "Ledger state file"
          (Consensus.decodeExtLedgerState
            (decodeDisk codecConfig)
            (decodeDisk codecConfig)
            (decodeDisk codecConfig))
        . LBS.fromStrict

-- Get a list of the ledger state files order most recent
listLedgerStateFilesOrdered :: LedgerStateDir -> IO [LedgerStateFile]
listLedgerStateFilesOrdered dir = do
    files <- filter isLedgerStateFile <$> listDirectory (unLedgerStateDir dir)
    pure . List.sortBy revSlotNoOrder $ mapMaybe (parseLedgerStateFileName dir) files
  where
    isLedgerStateFile :: FilePath -> Bool
    isLedgerStateFile fp = takeExtension fp == ".lstate"

    revSlotNoOrder :: LedgerStateFile -> LedgerStateFile -> Ordering
    revSlotNoOrder a b = compare (lsfSlotNo b) (lsfSlotNo a)

writeLedgerState :: LedgerEnv -> Maybe LedgerDB -> IO ()
writeLedgerState env mLedgerDb = atomically $ writeTVar (leStateVar env) mLedgerDb

-- | Remove given file path and ignore any IOEXceptions.
safeRemoveFile :: FilePath -> IO ()
safeRemoveFile fp = handle (\(_ :: IOException) -> pure ()) $ removeFile fp

getPoolParams :: GodxLedgerState -> Set.Set (KeyHash 'StakePool StandardCrypto)
getPoolParams st =
    case ledgerState $ clsState st of
      LedgerStateCole _ -> Set.empty
      LedgerStateSophie sts -> getPoolParamsSophie sts
      LedgerStateAllegra sts -> getPoolParamsSophie sts
      LedgerStateJen sts -> getPoolParamsSophie sts
      LedgerStateAurum ats -> getPoolParamsSophie ats

getPoolParamsSophie
    :: forall era. (Crypto era ~ StandardCrypto)
    => LedgerState (SophieBlock era)
    -> Set.Set (KeyHash 'StakePool StandardCrypto)
getPoolParamsSophie lState =
  Map.keysSet $ Sophie._pParams $ Sophie._pstate $ Sophie._delegationState
              $ Sophie.esLState $ Sophie.nesEs $ Consensus.sophieLedgerState lState

-- We only compute 'GodxPots' for later eras. This is a time consuming
-- function and we only want to run it on epoch boundaries.
getGodxPots :: GodxLedgerState -> Maybe Sophie.GodxPots
getGodxPots st =
    case ledgerState $ clsState st of
      LedgerStateCole _ -> Nothing
      LedgerStateSophie sts -> Just $ totalGodxPots sts
      LedgerStateAllegra sta -> Just $ totalGodxPots sta
      LedgerStateJen stm -> Just $ totalGodxPots stm
      LedgerStateAurum sta -> Just $ totalGodxPots sta

ledgerEpochNo :: LedgerEnv -> GodxLedgerState -> EpochNo
ledgerEpochNo env cls =
    case ledgerTipSlot (ledgerState (clsState cls)) of
      Origin -> 0 -- An empty chain is in epoch 0
      NotOrigin slot ->
        case runExcept $ epochInfoEpoch epochInfo slot of
          Left err -> panic $ "ledgerEpochNo: " <> textShow err
          Right en -> en
  where
    epochInfo :: EpochInfo (Except Consensus.PastHorizonException)
    epochInfo = epochInfoLedger (configLedger $ topLevelConfig env) (hardForkLedgerStatePerEra . ledgerState $ clsState cls)

-- Like 'Consensus.tickThenReapply' but also checks that the previous hash from the block matches
-- the head hash of the ledger state.
tickThenReapplyCheckHash
    :: ExtLedgerCfg GodxBlock -> GodxBlock
    -> ExtLedgerState GodxBlock
    -> Either Text (LedgerResult (ExtLedgerState GodxBlock) (ExtLedgerState GodxBlock))
tickThenReapplyCheckHash cfg block lsb =
  if blockPrevHash block == ledgerTipHash (ledgerState lsb)
    then Right $ tickThenReapplyLedgerResult cfg block lsb
    else Left $ mconcat
                  [ "Ledger state hash mismatch. Ledger head is slot "
                  , textShow (unSlotNo $ fromWithOrigin (SlotNo 0) (ledgerTipSlot $ ledgerState lsb))
                  , " hash ", renderByteArray (Godx.unChainHash (ledgerTipHash $ ledgerState lsb))
                  , " but block previous hash is "
                  , renderByteArray (Godx.unChainHash $ blockPrevHash block)
                  , " and block current hash is "
                  , renderByteArray (BSS.fromShort . Consensus.getOneEraHash $ blockHash block), "."
                  ]

totalGodxPots
    :: forall era. UsesValue era
    => LedgerState (SophieBlock era)
    -> Sophie.GodxPots
totalGodxPots = Sophie.totalGodxPotsES . Sophie.nesEs . Consensus.sophieLedgerState

getHeaderHash :: HeaderHash GodxBlock -> ByteString
getHeaderHash bh = BSS.fromShort (Consensus.getOneEraHash bh)

-- | This will fail if the state is not a 'LedgerStateAurum'
getAurumPParams :: GodxLedgerState -> PParams StandardAurum
getAurumPParams cls =
  case ledgerState $ clsState cls of
    LedgerStateAurum als -> esPp $ Sophie.nesEs $ Consensus.sophieLedgerState als
    _ -> panic "Expected LedgerStateAurum after an Aurum Block"

-- | This should be exposed by 'consensus'.
ledgerTipBlockNo :: ExtLedgerState blk -> WithOrigin BlockNo
ledgerTipBlockNo = fmap Consensus.annTipBlockNo . Consensus.headerStateTip . Consensus.headerState

