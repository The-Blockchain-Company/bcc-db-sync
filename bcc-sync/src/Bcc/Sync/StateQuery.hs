{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Godx.Sync.StateQuery
  ( StateQueryTMVar -- Opaque, so it cannot be misused.
  , getSlotDetails
  , localStateQueryHandler
  , newStateQueryTMVar
  ) where

import           Godx.BM.Trace (Trace, logInfo)

import           Godx.Slotting.Slot (SlotNo (..))

import           Godx.Db (textShow)

import           Godx.Sync.Api
import           Godx.Sync.Types

import           Godx.Prelude hiding (atomically)

import           Control.Monad.Class.MonadSTM.Strict (StrictTMVar, atomically, newEmptyTMVarIO,
                   putTMVar, takeTMVar)

import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)

import           Shardagnostic.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..),
                   SystemStart (..))
import           Shardagnostic.Consensus.Godx.Block (BlockQuery (QueryHardFork), GodxEras)
import           Shardagnostic.Consensus.Godx.Node ()
import           Shardagnostic.Consensus.HardFork.Combinator.Basics (HardForkBlock (..))
import           Shardagnostic.Consensus.HardFork.Combinator.Ledger.Query
                   (QueryHardFork (GetInterpreter))
import           Shardagnostic.Consensus.HardFork.History.Qry (Expr (..), Interpreter,
                   PastHorizonException, Qry, interpretQuery, qryFromExpr, slotToEpoch')
import           Shardagnostic.Consensus.Ledger.Query (Query (..))
import           Shardagnostic.Consensus.Sophie.Protocol (StandardCrypto)

import           Shardagnostic.Network.Block (Point (..))
import           Shardagnostic.Network.Protocol.LocalStateQuery.Client (ClientStAcquired (..),
                   ClientStAcquiring (..), ClientStIdle (..), ClientStQuerying (..),
                   LocalStateQueryClient (..))
import qualified Shardagnostic.Network.Protocol.LocalStateQuery.Client as StateQuery
import           Shardagnostic.Network.Protocol.LocalStateQuery.Type (AcquireFailure)

import           System.IO.Unsafe (unsafePerformIO)

newtype StateQueryTMVar blk result = StateQueryTMVar
  { unStateQueryTMVar ::
      StrictTMVar IO
        ( Query blk result
        , StrictTMVar IO (Either AcquireFailure result)
        )
  }

newStateQueryTMVar :: IO (StateQueryTMVar blk result)
newStateQueryTMVar = StateQueryTMVar <$> newEmptyTMVarIO


-- Get the requested slot details using a history interpreter stashed in an IORef.
-- If the history interpreter does not exist, get one.
-- If the existing history interpreter returns an error, get a new one and try again.
getSlotDetails
    :: Trace IO Text -> SyncEnv
    -> StateQueryTMVar (HardForkBlock (GodxEras StandardCrypto)) (Interpreter (GodxEras StandardCrypto))
    -> SlotNo
    -> IO SlotDetails
getSlotDetails tracer env queryVar slot = do
    einterp1 <- maybe (getHistoryInterpreter tracer queryVar) pure =<< readIORef historyInterpVar
    case evalSlotDetails einterp1 of
      Right sd -> insertCurrentTime sd
      Left _ -> do
        einterp2 <- getHistoryInterpreter tracer queryVar
        case evalSlotDetails einterp2 of
          Left err -> panic $ "getSlotDetails: " <> textShow err
          Right sd -> insertCurrentTime sd
  where
    evalSlotDetails :: Interpreter (GodxEras StandardCrypto) -> Either PastHorizonException SlotDetails
    evalSlotDetails interp =
      interpretQuery interp (querySlotDetails (envSystemStart env) slot)

    insertCurrentTime :: SlotDetails -> IO SlotDetails
    insertCurrentTime sd = do
      time <- getCurrentTime
      pure $ sd { sdCurrentTime = time }

-- -------------------------------------------------------------------------------------------------

{-# NOINLINE historyInterpVar #-}
historyInterpVar :: IORef (Maybe (Interpreter (GodxEras StandardCrypto)))
historyInterpVar = unsafePerformIO $ newIORef Nothing

getHistoryInterpreter
    :: Trace IO Text
    -> StateQueryTMVar (HardForkBlock (GodxEras StandardCrypto)) (Interpreter (GodxEras StandardCrypto))
    -> IO (Interpreter (GodxEras StandardCrypto))
getHistoryInterpreter tracer queryVar = do
  respVar <- newEmptyTMVarIO
  atomically $ putTMVar (unStateQueryTMVar queryVar) (BlockQuery $ QueryHardFork GetInterpreter, respVar)
  res <- atomically $ takeTMVar respVar
  case res of
    Left err ->
      panic $ "getHistoryInterpreter: " <> textShow err
    Right interp -> do
      logInfo tracer "getHistoryInterpreter: acquired"
      writeIORef historyInterpVar $ Just interp
      pure interp


-- This is called during the ChainSync setup and loops forever. Queries can be posted to
-- it and responses retrieved via a TVar.
localStateQueryHandler
    :: forall block result a
    . StateQueryTMVar block result
    -> LocalStateQueryClient block (Point block) (Query block) IO a
localStateQueryHandler (StateQueryTMVar reqVar) =
    LocalStateQueryClient idleState
  where
    idleState :: IO (StateQuery.ClientStIdle block (Point block) (Query block) IO a)
    idleState = do
      (query, respVar) <- atomically $ takeTMVar reqVar
      pure .
        SendMsgAcquire Nothing $
          ClientStAcquiring
            { recvMsgAcquired =
                pure . SendMsgQuery query $
                  ClientStQuerying
                    { recvMsgResult = \result -> do
                        atomically $ putTMVar respVar (Right result)
                        pure $ SendMsgRelease idleState
                    }
            , recvMsgFailure = \failure -> do
                atomically $ putTMVar respVar (Left failure)
                idleState
            }

-- -------------------------------------------------------------------------------------------------

-- TODO: Switch back to the old version of this when this is fixed:
-- https://github.com/The-Blockchain-Company/bcc-db-sync/issues/276
querySlotDetails :: SystemStart -> SlotNo -> Qry SlotDetails
querySlotDetails start absSlot = do
  absTime <- qryFromExpr $
                ELet (EAbsToRelSlot (ELit absSlot)) $ \ relSlot ->
                ELet (ERelSlotToTime (EVar relSlot)) $ \ relTime ->
                ELet (ERelToAbsTime (EVar relTime)) $ \ absTime ->
                EVar absTime
  (absEpoch, slotInEpoch) <- slotToEpoch' absSlot
  epochSize <- qryFromExpr $ EEpochSize (ELit absEpoch)
  let time = relToUTCTime start absTime
  -- Set sdCurrentTime below and over write that above.
  pure $ SlotDetails time time absEpoch (EpochSlot slotInEpoch) epochSize

relToUTCTime :: SystemStart -> RelativeTime -> UTCTime
relToUTCTime (SystemStart start) (RelativeTime rel) = addUTCTime rel start
