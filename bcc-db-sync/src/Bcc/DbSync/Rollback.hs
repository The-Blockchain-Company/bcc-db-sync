{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Godx.DbSync.Rollback
  ( rollbackToPoint
  , unsafeRollback
  ) where

import           Godx.Prelude
import qualified Data.ByteString.Short as BSS

import           Godx.BM.Trace (Trace, logInfo)

import qualified Godx.Db as DB

import           Godx.DbSync.Era.Util

import           Godx.Sync.Error
import           Godx.Sync.Types
import           Godx.Sync.Util

import qualified Data.List as List
import           Database.Persist.Sql (SqlBackend)

import           Shardagnostic.Consensus.HardFork.Combinator.AcrossEras (getOneEraHash)

import           Shardagnostic.Network.Block
import           Shardagnostic.Network.Point

-- Rollbacks are done in an Era generic way based on the 'Point' we are
-- rolling back to.
rollbackToPoint :: SqlBackend -> Trace IO Text -> GodxPoint -> IO (Either SyncNodeError ())
rollbackToPoint backend trce point =
    DB.runDbtbcoNoLogging backend $ runExceptT action
  where
    action :: MonadIO m => ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    action = do
        liftIO . logInfo trce $ "Rolling back to " <> renderPoint point
        xs <- lift $ slotsToDelete (pointSlot point)
        unless (null xs) $
          -- there may be more deleted blocks than slots, because ebbs don't have
          -- a slot. We can only make an estimation here.
          liftIO . logInfo trce $
              mconcat
                [ "Deleting ", textShow (length xs), " blocks up to slot "
                , textShow (unSlotNo $ List.head xs)
                ]
        -- We delete the block right after the point we rollback to. This delete
        -- should cascade to the rest of the chain.
        prevId <- liftLookupFail "Rollback.rollbackToPoint" $ queryBlockId point
        deleted <- lift $ DB.deleteCascadeAfter prevId
        liftIO . logInfo trce $
                    if deleted
                      then "Blocks deleted"
                      else "No blocks need to be deleted"

    slotsToDelete :: MonadIO m => WithOrigin SlotNo -> ReaderT SqlBackend m [SlotNo]
    slotsToDelete wosl =
      case wosl of
        Origin -> DB.querySlotNos
        At sl -> DB.querySlotNosGreaterThan (unSlotNo sl)

    queryBlockId :: MonadIO m => Point GodxBlock -> ReaderT SqlBackend m (Either DB.LookupFail DB.BlockId)
    queryBlockId pnt =
      case getPoint pnt of
        Origin -> DB.queryGenesis
        At blk -> DB.queryBlockId (BSS.fromShort . getOneEraHash $ blockPointHash blk)

-- For testing and debugging.
unsafeRollback :: Trace IO Text -> SlotNo -> IO (Either SyncNodeError ())
unsafeRollback trce slotNo = do
  logInfo trce $ "Forced rollback to slot " <> textShow (unSlotNo slotNo)
  Right <$> DB.runDbNoLogging (void $ DB.deleteCascadeSlotNo slotNo)
