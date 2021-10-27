{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Godx.Db.Run
  ( getBackendGhci
  , ghciDebugQuery
  , runDbHandleLogger
  , runDbtbcoLogging
  , runDbtbcoNoLogging
  , runDbNoLogging
  , runDbStdoutLogging
  , runtbcoLogging
  , transactionCommit
  ) where

import           Godx.BM.Data.LogItem (LOContent (..), LogObject (..), PrivacyAnnotation (..),
                   mkLOMeta)
import           Godx.BM.Data.Severity (Severity (..))
import           Godx.BM.Trace (Trace)

import           Godx.Db.PGConfig

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (LogLevel (..), LogSource, LoggingT, NoLoggingT,
                   defaultLogStr, runLoggingT, runNoLoggingT, runStdoutLoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (MonadUnliftIO)
import           Control.Tracer (traceWith)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Pool as Pool
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy.Builder as LazyText
import qualified Data.Text.Lazy.IO as LazyText

import           Database.Esqueleto.Internal.Internal (Mode (SELECT), SqlSelect, initialIdentState,
                   toRawSql)
import           Database.Esqueleto.Legacy (SqlQuery)

import           Database.Persist.Postgresql (SqlBackend, openSimpleConn, withPostgresqlPool)
import           Database.Persist.Sql (IsolationLevel (..), runSqlConnWithIsolation,
                   transactionSaveWithIsolation)
import           Database.PostgreSQL.Simple (connectPostgreSQL)

import           Language.Haskell.TH.Syntax (Loc)

import           System.IO (Handle, stdout)
import           System.Log.FastLogger (LogStr, fromLogStr)

-- | Run a DB action logging via the provided Handle.
runDbHandleLogger :: Handle -> ReaderT SqlBackend (LoggingT IO) a -> IO a
runDbHandleLogger logHandle dbAction = do
    pgconf <- readPGPassFileEnv Nothing
    runHandleLoggerT .
      withPostgresqlPool (toConnectionString pgconf) defPoolCount $ \pool ->
        Pool.withResource pool $ \backend ->
          -- The 'runSqlConnWithIsolation' function starts a transaction, runs the 'dbAction'
          -- and then commits the transaction.
          runSqlConnWithIsolation dbAction backend Serializable
  where
    runHandleLoggerT :: LoggingT m a -> m a
    runHandleLoggerT action =
      runLoggingT action logOut

    logOut :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    logOut loc src level msg =
      BS.hPutStrLn logHandle . fromLogStr $ defaultLogStr loc src level msg

-- | Run a DB action logging via tbco-monitoring-framework.
runDbtbcoLogging :: MonadUnliftIO m => SqlBackend -> Trace IO Text -> ReaderT SqlBackend (LoggingT m) b -> m b
runDbtbcoLogging backend tracer dbAction = do
    runtbcoLogging tracer $ runSqlConnWithIsolation dbAction backend Serializable

-- | Run a DB action logging via tbco-monitoring-framework.
runDbtbcoNoLogging :: MonadUnliftIO m => SqlBackend -> ReaderT SqlBackend (NoLoggingT m) a -> m a
runDbtbcoNoLogging backend action = do
    runNoLoggingT $ runSqlConnWithIsolation action backend Serializable

runtbcoLogging :: Trace IO Text -> LoggingT m a -> m a
runtbcoLogging tracer action =
    runLoggingT action totbcoLog
  where
    totbcoLog :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    totbcoLog _loc _src level msg = do
      meta <- mkLOMeta (totbcoSeverity level) Public
      traceWith tracer
        (name, LogObject name meta (LogMessage . Text.decodeLatin1 $ fromLogStr msg))

    name :: Text
    name = "db-sync"

    totbcoSeverity :: LogLevel -> Severity
    totbcoSeverity =
      \case
        LevelDebug -> Debug
        LevelInfo -> Info
        LevelWarn -> Warning
        LevelError -> Error
        LevelOther _ -> Error

-- | Run a DB action without any logging. Mainly for tests.
runDbNoLogging :: (MonadBaseControl IO m, MonadUnliftIO m) => ReaderT SqlBackend (NoLoggingT m) a -> m a
runDbNoLogging action = do
  pgconfig <- liftIO $ readPGPassFileEnv Nothing
  runNoLoggingT .
    withPostgresqlPool (toConnectionString pgconfig) defPoolCount $ \pool ->
      Pool.withResource pool $ \backend ->
        runSqlConnWithIsolation action backend Serializable

-- | Run a DB action with stdout logging. Mainly for debugging.
runDbStdoutLogging :: ReaderT SqlBackend (LoggingT IO) b -> IO b
runDbStdoutLogging action = do
  pgconfig <- readPGPassFileEnv Nothing
  runStdoutLoggingT .
    withPostgresqlPool (toConnectionString pgconfig) defPoolCount $ \pool ->
      Pool.withResource pool $ \backend ->
        runSqlConnWithIsolation action backend Serializable

getBackendGhci :: IO SqlBackend
getBackendGhci = do
  pgconfig <- readPGPassFileEnv Nothing
  connection <- connectPostgreSQL (toConnectionString pgconfig)
  openSimpleConn (defaultOutput stdout) connection

ghciDebugQuery :: SqlSelect a r => SqlQuery a -> IO ()
ghciDebugQuery query = do
  pgconfig <- readPGPassFileEnv Nothing
  runStdoutLoggingT . withPostgresqlPool (toConnectionString pgconfig) defPoolCount $ \pool ->
    Pool.withResource pool $ \backend -> do
      let (sql,params) = toRawSql SELECT (backend, initialIdentState) query
      liftIO $ do
        LazyText.putStr $ LazyText.toLazyText sql
        print params

transactionCommit :: MonadIO m => ReaderT SqlBackend m ()
transactionCommit = transactionSaveWithIsolation Serializable

-- -------------------------------------------------------------------------------------------------

-- from Control.Monad.Logger, wasnt exported
defaultOutput :: Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
defaultOutput h loc src level msg =
    BS.hPutStr h $ logStrBS loc src level msg
  where
    logStrBS :: Loc -> LogSource -> LogLevel -> LogStr -> BS.ByteString
    logStrBS a b c d = fromLogStr $ defaultLogStr a b c d

-- Currently have two threads operating on the database, so 10 should be more than enough.
defPoolCount :: Int
defPoolCount = 10
