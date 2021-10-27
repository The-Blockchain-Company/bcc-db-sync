{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Godx.DbSync
  ( ConfigFile (..)
  , SyncCommand (..)
  , SyncNodeParams (..)
  , SyncNodePlugin (..)
  , GenesisFile (..)
  , LedgerStateDir (..)
  , NetworkName (..)
  , SocketPath (..)
  , DB.MigrationDir (..)

  , defDbSyncNodePlugin
  , runDbSyncNode
  ) where

import           Godx.Prelude hiding (Nat, option, (%))

import           Control.Monad.Trans.Maybe (MaybeT (..))

import           Godx.Slotting.Slot (EpochNo (..), SlotNo (..))

import           Godx.BM.Trace (Trace, logError, logInfo)

import qualified Godx.Db as DB

import           Godx.DbSync.Era (insertValidateGenesisDist)
import           Godx.DbSync.Plugin.Default (defDbSyncNodePlugin)
import           Godx.DbSync.Rollback (unsafeRollback)
import           Godx.Sync.Database (runDbThread)

import           Godx.Sync (Block (..), MetricSetters, SyncDataLayer (..), SyncNodePlugin (..),
                   configureLogging, runSyncNode)
import           Godx.Sync.Config.Types (ConfigFile (..), GenesisFile (..), LedgerStateDir (..),
                   MigrationDir (..), NetworkName (..), SocketPath (..), SyncCommand (..),
                   SyncNodeParams (..))
import           Godx.Sync.Tracing.ToObjectOrphans ()

import           Control.Monad.Extra (whenJust)

import           Database.Persist.Postgresql (withPostgresqlConn)

import           Database.Persist.Sql (SqlBackend)

import           Shardagnostic.Network.Block (BlockNo (..))

runDbSyncNode :: MetricSetters -> (SqlBackend -> SyncNodePlugin) -> [(Text, Text)] -> SyncNodeParams -> IO ()
runDbSyncNode metricsSetters mkPlugin knownMigrations params = do

    -- Read the PG connection info
    pgConfig <- DB.readPGPassFileEnv Nothing

    trce <- configureLogging params "db-sync-node"

    orDieWithLog DB.renderMigrationValidateError trce $ DB.validateMigrations dbMigrationDir knownMigrations

    logInfo trce "Schema migration files validated"

    logInfo trce "Running database migrations"

    DB.runMigrations pgConfig True dbMigrationDir (Just $ DB.LogFileDir "/tmp")

    let connectionString = DB.toConnectionString pgConfig

    DB.runtbcoLogging trce $ withPostgresqlConn connectionString $ \backend ->
      lift $ do
        -- For testing and debugging.
        whenJust (enpMaybeRollback params) $ \ slotNo ->
          void $ unsafeRollback trce slotNo

        -- The separation of `bcc-db` and `bcc-sync` is such a *HUGE* pain in the neck.
        runSyncNode (mkSyncDataLayer trce backend) metricsSetters trce (mkPlugin backend)
            params (insertValidateGenesisDist backend) runDbThread
  where
    -- This is only necessary because `bcc-db` and `bcc-sync` both define
    -- this newtype, but the later does not depend on the former.
    dbMigrationDir :: DB.MigrationDir
    dbMigrationDir = DB.MigrationDir $ unMigrationDir (enpMigrationDir params)

-- -------------------------------------------------------------------------------------------------

-- The base @DataLayer@.
mkSyncDataLayer :: Trace IO Text -> SqlBackend -> SyncDataLayer
mkSyncDataLayer trce backend =
  SyncDataLayer
    { sdlGetSlotHash = DB.runDbtbcoLogging backend trce . DB.querySlotHash
    , sdlGetLatestBlock =
        runMaybeT $ do
          block <- MaybeT $ DB.runDbNoLogging DB.queryLatestBlock
          -- The EpochNo, SlotNo and BlockNo can only be zero for the Cole
          -- era, but we need to make the types match, hence `fromMaybe`.
          pure $ Block
                  { bHash = DB.blockHash block
                  , bEpochNo = EpochNo . fromMaybe 0 $ DB.blockEpochNo block
                  , bSlotNo = SlotNo . fromMaybe 0 $ DB.blockSlotNo block
                  , bBlockNo = BlockNo . fromMaybe 0 $ DB.blockBlockNo block
                  }
    , sdlGetLatestSlotNo = SlotNo <$> DB.runDbNoLogging DB.queryLatestSlotNo
    }

-- Log error to Trace and panic.
orDieWithLog :: (t -> Text) -> Trace IO Text -> ExceptT t IO () -> IO ()
orDieWithLog render trce e = do
  runExceptT e >>= \case
    Left errors -> do
      let errorStr = render errors
      liftIO $ logError trce errorStr
      panic errorStr
    Right () -> pure ()

