{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Godx.Sync
  ( ConfigFile (..)
  , SyncCommand (..)
  , SyncNodeParams (..)
  , SyncNodePlugin (..)
  , GenesisFile (..)
  , LedgerStateDir (..)
  , NetworkName (..)
  , SocketPath (..)

  , SyncDataLayer (..)
  , MetricSetters (..)
  , nullMetricSetters
  , Block (..)
  , SyncEnv (..)

  , configureLogging
  , runSyncNode
  ) where

import           Godx.Prelude hiding (Meta, Nat, option, (%))

import           Control.Tracer (Tracer)

import           Godx.BM.Data.Tracer (ToLogObject (..))
import           Godx.BM.Trace (Trace, appendName, logError, logInfo)
import qualified Godx.BM.Trace as Logging

import qualified Godx.Chain.Genesis as Cole
import           Godx.Client.Subscription (subscribe)
import qualified Godx.Crypto as Crypto

import           Godx.Slotting.Slot (SlotNo (..), WithOrigin (..))

import           Godx.Sync.Api
import           Godx.Sync.Config
import           Godx.Sync.Database (runDbStartup)
import           Godx.Sync.DbAction
import           Godx.Sync.Error
import           Godx.Sync.Metrics
import           Godx.Sync.Plugin (SyncNodePlugin (..))
import           Godx.Sync.StateQuery (StateQueryTMVar, getSlotDetails, localStateQueryHandler,
                   newStateQueryTMVar)
import           Godx.Sync.Tracing.ToObjectOrphans ()
import           Godx.Sync.Types
import           Godx.Sync.Util

import qualified Codec.CBOR.Term as CBOR

import           Control.Monad.Trans.Except.Exit (orDie)

import qualified Data.ByteString.Lazy as BSL
import           Data.Functor.Contravariant (contramap)
import qualified Data.Text as Text

import           Network.Mux (MuxTrace, WithMuxBearer)
import           Network.Mux.Types (MuxMode (..))

import           Network.TypedProtocol.Pipelined (N (..), Nat (Succ, Zero))
import           Shardagnostic.Network.Driver.Simple (runPipelinedPeer)

import           Shardagnostic.Consensus.Block.Abstract (CodecConfig)
import           Shardagnostic.Consensus.Cole.Ledger.Config (mkColeCodecConfig)
import           Shardagnostic.Consensus.Cole.Node ()
import           Shardagnostic.Consensus.Godx.Block (GodxEras, CodecConfig (..))
import           Shardagnostic.Consensus.Godx.Node ()
import           Shardagnostic.Consensus.HardFork.History.Qry (Interpreter)
import           Shardagnostic.Consensus.Network.NodeToClient (ClientCodecs, cChainSyncCodec,
                   cStateQueryCodec, cTxSubmissionCodec)
import           Shardagnostic.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import           Shardagnostic.Consensus.Sophie.Ledger.Config (CodecConfig (SophieCodecConfig))
import           Shardagnostic.Consensus.Sophie.Protocol (StandardCrypto)

import           Shardagnostic.Network.Block (BlockNo (..), Point (..), Tip (..), blockNo, genesisPoint,
                   getTipBlockNo)
import           Shardagnostic.Network.Mux (MuxPeer (..), RunMiniProtocol (..))
import           Shardagnostic.Network.NodeToClient (ClientSubscriptionParams (..), ConnectionId,
                   ErrorPolicyTrace (..), Handshake, IOManager, LocalAddress,
                   NetworkSubscriptionTracers (..), NodeToClientProtocols (..), TraceSendRecv,
                   WithAddr (..), localSnocket, localTxSubmissionPeerNull, networkErrorPolicies,
                   withIOManager)
import qualified Shardagnostic.Network.NodeToClient.Version as Network

import           Shardagnostic.Network.Protocol.ChainSync.ClientPipelined
                   (ChainSyncClientPipelined (..), ClientPipelinedStIdle (..),
                   ClientPipelinedStIntersect (..), ClientStNext (..), chainSyncClientPeerPipelined,
                   recvMsgIntersectFound, recvMsgIntersectNotFound, recvMsgRollBackward,
                   recvMsgRollForward)
import           Shardagnostic.Network.Protocol.ChainSync.PipelineDecision (MkPipelineDecision,
                   PipelineDecision (..), pipelineDecisionLowHighMark, runPipelineDecision)
import           Shardagnostic.Network.Protocol.ChainSync.Type (ChainSync)
import           Shardagnostic.Network.Protocol.LocalStateQuery.Client (localStateQueryClientPeer)
import qualified Shardagnostic.Network.Snocket as Snocket
import           Shardagnostic.Network.Subscription (SubscriptionTrace)

import           System.Directory (createDirectoryIfMissing)


type InsertValidateGenesisFunction
    = Trace IO Text
    -> NetworkName
    -> GenesisConfig
    -> ExceptT SyncNodeError IO ()

type RunDBThreadFunction
    =  Trace IO Text
    -> SyncEnv
    -> MetricSetters
    -> SyncNodePlugin
    -> DbActionQueue
    -> IO ()

runSyncNode
    :: SyncDataLayer
    -> MetricSetters
    -> Trace IO Text
    -> SyncNodePlugin
    -> SyncNodeParams
    -> InsertValidateGenesisFunction
    -> RunDBThreadFunction
    -> IO ()
runSyncNode dataLayer metricsSetters trce plugin enp insertValidateGenesisDist runDBThreadFunction =
  withIOManager $ \iomgr -> do

    let configFile = enpConfigFile enp
    enc <- readSyncNodeConfig configFile

    createDirectoryIfMissing True (unLedgerStateDir $ enpLedgerStateDir enp)

    logInfo trce $ "Using cole genesis file from: " <> (show . unGenesisFile $ dncColeGenesisFile enc)
    logInfo trce $ "Using sophie genesis file from: " <> (show . unGenesisFile $ dncSophieGenesisFile enc)
    logInfo trce $ "Using aurum genesis file from: " <> (show . unGenesisFile $ dncAurumGenesisFile enc)

    orDie renderSyncNodeError $ do
      genCfg <- readGodxGenesisConfig enc
      logProtocolMagicId trce $ genesisProtocolMagicId genCfg

      -- If the DB is empty it will be inserted, otherwise it will be validated (to make
      -- sure we are on the right chain).
      insertValidateGenesisDist trce (dncNetworkName enc) genCfg

        -- Must run plugin startup after the genesis distribution has been inserted/validate.
      liftIO $ runDbStartup trce plugin
      case genCfg of
          GenesisGodx _ bCfg _sCfg _aCfg -> do
            syncEnv <- ExceptT $ mkSyncEnvFromConfig dataLayer trce (enpLedgerStateDir enp) genCfg
            liftIO $ runSyncNodeNodeClient metricsSetters syncEnv iomgr trce plugin
                        runDBThreadFunction (bccCodecConfig bCfg) (enpSocketPath enp)
  where
    bccCodecConfig :: Cole.Config -> CodecConfig GodxBlock
    bccCodecConfig cfg =
      GodxCodecConfig
        (mkColeCodecConfig cfg)
        SophieCodecConfig
        SophieCodecConfig -- Allegra
        SophieCodecConfig -- Jen
        SophieCodecConfig -- Aurum

-- -------------------------------------------------------------------------------------------------

runSyncNodeNodeClient
    :: MetricSetters
    -> SyncEnv
    -> IOManager
    -> Trace IO Text
    -> SyncNodePlugin
    -> RunDBThreadFunction
    -> CodecConfig GodxBlock
    -> SocketPath
    -> IO ()
runSyncNodeNodeClient metricsSetters env iomgr trce plugin runDBThreadFunction codecConfig (SocketPath socketPath) = do
  queryVar <- newStateQueryTMVar
  logInfo trce $ "localInitiatorNetworkApplication: connecting to node via " <> textShow socketPath
  void $ subscribe
    (localSnocket iomgr socketPath)
    codecConfig
    (envNetworkMagic env)
    networkSubscriptionTracers
    clientSubscriptionParams
    (dbSyncProtocols trce env metricsSetters plugin queryVar runDBThreadFunction)
  where
    clientSubscriptionParams =
      ClientSubscriptionParams
        { cspAddress = Snocket.localAddressFromPath socketPath
        , cspConnectionAttemptDelay = Nothing
        , cspErrorPolicies = networkErrorPolicies <> consensusErrorPolicy (Proxy @GodxBlock)
        }

    networkSubscriptionTracers =
      NetworkSubscriptionTracers
        { nsMuxTracer = muxTracer
        , nsHandshakeTracer = handshakeTracer
        , nsErrorPolicyTracer = errorPolicyTracer
        , nsSubscriptionTracer = subscriptionTracer
        }

    errorPolicyTracer :: Tracer IO (WithAddr LocalAddress ErrorPolicyTrace)
    errorPolicyTracer = toLogObject $ appendName "ErrorPolicy" trce

    muxTracer :: Show peer => Tracer IO (WithMuxBearer peer MuxTrace)
    muxTracer = toLogObject $ appendName "Mux" trce

    subscriptionTracer :: Tracer IO (Identity (SubscriptionTrace LocalAddress))
    subscriptionTracer = toLogObject $ appendName "Subscription" trce

    handshakeTracer :: Tracer IO (WithMuxBearer
                          (ConnectionId LocalAddress)
                          (TraceSendRecv (Handshake Network.NodeToClientVersion CBOR.Term)))
    handshakeTracer = toLogObject $ appendName "Handshake" trce

dbSyncProtocols
    :: Trace IO Text -> SyncEnv -> MetricSetters -> SyncNodePlugin
    -> StateQueryTMVar GodxBlock (Interpreter (GodxEras StandardCrypto))
    -> RunDBThreadFunction -> Network.NodeToClientVersion -> ClientCodecs GodxBlock IO
    -> ConnectionId LocalAddress
    -> NodeToClientProtocols 'InitiatorMode BSL.ByteString IO () Void
dbSyncProtocols trce env metricsSetters plugin queryVar runDBThreadFunction version codecs _connectionId =
    NodeToClientProtocols
      { localChainSyncProtocol = localChainSyncPtcl
      , localTxSubmissionProtocol = dummylocalTxSubmit
      , localStateQueryProtocol = localStateQuery
      }
  where
    localChainSyncTracer :: Tracer IO (TraceSendRecv (ChainSync GodxBlock(Point GodxBlock) (Tip GodxBlock)))
    localChainSyncTracer = toLogObject $ appendName "ChainSync" trce

    localChainSyncPtcl :: RunMiniProtocol 'InitiatorMode BSL.ByteString IO () Void
    localChainSyncPtcl = InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
      liftIO . logException trce "ChainSyncWithBlocksPtcl: " $ do
        logInfo trce "Starting chainSyncClient"

        when (version < minVersion) $ do
          logError trce versionErrorMsg
          throwIO $ ErrorCall (Text.unpack versionErrorMsg)

        latestPoints <- getLatestPoints env
        currentTip <- getCurrentTipBlockNo (envDataLayer env)
        logDbState (envDataLayer env) trce
        actionQueue <- newDbActionQueue

        race_
            (runDBThreadFunction trce env metricsSetters plugin actionQueue)
            (runPipelinedPeer
                localChainSyncTracer
                (cChainSyncCodec codecs)
                channel
                (chainSyncClientPeerPipelined
                    $ chainSyncClient plugin metricsSetters trce env queryVar latestPoints currentTip actionQueue)
            )

        atomically $ writeDbActionQueue actionQueue DbFinish
        -- We should return leftover bytes returned by 'runPipelinedPeer', but
        -- client application do not care about them (it's only important if one
        -- would like to restart a protocol on the same mux and thus bearer).
        pure ((), Nothing)

    dummylocalTxSubmit :: RunMiniProtocol 'InitiatorMode BSL.ByteString IO () Void
    dummylocalTxSubmit = InitiatorProtocolOnly $ MuxPeer
        Logging.nullTracer
        (cTxSubmissionCodec codecs)
        localTxSubmissionPeerNull

    localStateQuery :: RunMiniProtocol 'InitiatorMode BSL.ByteString IO () Void
    localStateQuery =
      InitiatorProtocolOnly $ MuxPeer
        (contramap (Text.pack . show) . toLogObject $ appendName "local-state-query" trce)
        (cStateQueryCodec codecs)
        (localStateQueryClientPeer (localStateQueryHandler queryVar))

    versionErrorMsg :: Text
    versionErrorMsg = Text.concat
        [ "The bcc-node version is too old. Please upgrade to a compatible "
        , "bcc-node version. The db-sync requires a node that supports "
        , textShow minVersion
        , ", the one in use only supports "
        , textShow version
        ]

    minVersion :: Network.NodeToClientVersion
    minVersion = Network.NodeToClientV_8

logDbState :: SyncDataLayer -> Trace IO Text -> IO ()
logDbState dataLayer trce = do
    let getLatestBlock = sdlGetLatestBlock dataLayer
    mblk <- getLatestBlock
    case mblk of
      Nothing -> logInfo trce "Godx.Db is empty"
      Just block ->
          logInfo trce $ Text.concat
                  [ "Godx.Db tip is at "
                  , showTip block
                  ]
  where
    showTip :: Block -> Text
    showTip blk =
      mconcat
        [ "slot ", textShow (unSlotNo $ bSlotNo blk)
        , ", block ", textShow (unBlockNo $ bBlockNo blk)
        ]

getCurrentTipBlockNo :: SyncDataLayer -> IO (WithOrigin BlockNo)
getCurrentTipBlockNo dataLayer = do
    maybeTip <- sdlGetLatestBlock dataLayer
    case maybeTip of
      Just tip -> pure $ At (bBlockNo tip)
      Nothing -> pure Origin

-- | 'ChainSyncClient' which traces received blocks and ignores when it
-- receives a request to rollbackwar.  A real wallet client should:
--
--  * at startup send the list of points of the chain to help synchronise with
--    the node;
--  * update its state when the client receives next block or is requested to
--    rollback, see 'clientStNext' below.
--
-- When an intersect with the node is found, we are sure that the next message
-- will trigger the 'recvMsgRollBackward', so this is where we actually handle
-- any necessary rollback. This means that at this point, the 'currentTip' may not
-- be correct. This is not an issue, because we only use it for performance reasons
-- in the pipeline policy.
chainSyncClient
    :: SyncNodePlugin
    -> MetricSetters
    -> Trace IO Text
    -> SyncEnv
    -> StateQueryTMVar GodxBlock (Interpreter (GodxEras StandardCrypto))
    -> [Point GodxBlock]
    -> WithOrigin BlockNo
    -> DbActionQueue
    -> ChainSyncClientPipelined GodxBlock (Point GodxBlock) (Tip GodxBlock) IO ()
chainSyncClient _plugin metricsSetters trce env queryVar latestPoints currentTip actionQueue = do
    ChainSyncClientPipelined $ pure $ clientPipelinedStIdle currentTip latestPoints
  where
    clientPipelinedStIdle
        :: WithOrigin BlockNo -> [GodxPoint]
        -> ClientPipelinedStIdle 'Z GodxBlock (Point GodxBlock) (Tip GodxBlock) IO ()
    clientPipelinedStIdle clintTip points =
      -- Notify the core node about the our latest points at which we are
      -- synchronised.  This client is not persistent and thus it just
      -- synchronises from the genesis block.  A real implementation should send
      -- a list of points up to a point which is k blocks deep.
      SendMsgFindIntersect
        (if null points then [genesisPoint] else points)
        ClientPipelinedStIntersect
          { recvMsgIntersectFound = \ _hdr tip -> pure $ go policy Zero clintTip (getTipBlockNo tip) Nothing
          , recvMsgIntersectNotFound = \tip -> pure $ goTip policy Zero clintTip tip Nothing
          }

    policy :: MkPipelineDecision
    policy = pipelineDecisionLowHighMark 1 50

    goTip :: MkPipelineDecision -> Nat n -> WithOrigin BlockNo -> Tip GodxBlock -> Maybe [GodxPoint]
          -> ClientPipelinedStIdle n GodxBlock (Point GodxBlock) (Tip GodxBlock) IO ()
    goTip mkPipelineDecision n clientTip serverTip mPoint =
      go mkPipelineDecision n clientTip (getTipBlockNo serverTip) mPoint

    go :: MkPipelineDecision -> Nat n -> WithOrigin BlockNo -> WithOrigin BlockNo -> Maybe [GodxPoint]
        -> ClientPipelinedStIdle n GodxBlock (Point GodxBlock) (Tip GodxBlock) IO ()
    go mkPipelineDecision n clientTip serverTip mPoint =
      case (mPoint, n, runPipelineDecision mkPipelineDecision n clientTip serverTip) of
        (Just points, _, _) -> drainThePipe n $ clientPipelinedStIdle clientTip points
        (_, _Zero, (Request, mkPipelineDecision')) ->
            SendMsgRequestNext clientStNext (pure clientStNext)
          where
            clientStNext = mkClientStNext $ goTip mkPipelineDecision' n
        (_, _, (Pipeline, mkPipelineDecision')) ->
          SendMsgRequestNextPipelined
            (go mkPipelineDecision' (Succ n) clientTip serverTip Nothing)
        (_, Succ n', (CollectOrPipeline, mkPipelineDecision')) ->
          CollectResponse
            (Just . pure $ SendMsgRequestNextPipelined $ go mkPipelineDecision' (Succ n) clientTip serverTip Nothing)
            (mkClientStNext $ goTip mkPipelineDecision' n')
        (_, Succ n', (Collect, mkPipelineDecision')) ->
          CollectResponse
            Nothing
            (mkClientStNext $ goTip mkPipelineDecision' n')

    mkClientStNext
        :: (WithOrigin BlockNo -> Tip GodxBlock -> Maybe [GodxPoint]
        -> ClientPipelinedStIdle n GodxBlock (Point GodxBlock) (Tip GodxBlock) IO ())
        -> ClientStNext n GodxBlock (Point GodxBlock) (Tip GodxBlock) IO ()
    mkClientStNext finish =
      ClientStNext
        { recvMsgRollForward = \blk tip ->
              logException trce "recvMsgRollForward: " $ do

                setNodeBlockHeight metricsSetters (getTipBlockNo tip)

                details <- getSlotDetails trce env queryVar (bccBlockSlotNo blk)
                newSize <- atomically $ do
                                writeDbActionQueue actionQueue $ mkDbApply blk details
                                lengthDbActionQueue actionQueue

                setDbQueueLength metricsSetters newSize

                pure $ finish (At (blockNo blk)) tip Nothing
        , recvMsgRollBackward = \point tip ->
              logException trce "recvMsgRollBackward: " $ do
                -- This will get the current tip rather than what we roll back to
                -- but will only be incorrect for a short time span.
                mPoints <- waitRollback actionQueue point
                newTip <- getCurrentTipBlockNo (envDataLayer env)
                pure $ finish newTip tip mPoints
        }

logProtocolMagicId :: Trace IO Text -> Crypto.ProtocolMagicId -> ExceptT SyncNodeError IO ()
logProtocolMagicId tracer pm =
  liftIO . logInfo tracer $ mconcat
    [ "NetworkMagic: ", textShow (Crypto.unProtocolMagicId pm)
    ]

drainThePipe
    :: Nat n -> ClientPipelinedStIdle 'Z GodxBlock (Point GodxBlock) (Tip GodxBlock) IO ()
    -> ClientPipelinedStIdle  n GodxBlock (Point GodxBlock) (Tip GodxBlock) IO ()
drainThePipe n0 client = go n0
  where
    go :: forall n'. Nat n'
       -> ClientPipelinedStIdle n' GodxBlock (Point GodxBlock) (Tip GodxBlock) IO ()
    go n =
      case n of
        Zero -> client
        Succ n' ->
          CollectResponse Nothing $
            ClientStNext
              { recvMsgRollForward  = \_hdr _tip -> pure $ go n'
              , recvMsgRollBackward = \_pt  _tip -> pure $ go n'
              }
