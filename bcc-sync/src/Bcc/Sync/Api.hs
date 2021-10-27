{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Godx.Sync.Api
  ( SyncEnv (..)
  , LedgerEnv (..)
  , SyncDataLayer (..)
  , mkSyncEnvFromConfig
  , verifyFilePoints
  , getLatestPoints
  ) where

import           Godx.Prelude (Proxy (..), catMaybes, find)

import           Godx.BM.Trace (Trace)

import qualified Godx.Ledger.BaseTypes as Ledger

import           Godx.Db (textShow)

import           Godx.Sync.Config.Godx
import           Godx.Sync.Config.Sophie
import           Godx.Sync.Config.Types
import           Godx.Sync.Error
import           Godx.Sync.LedgerState
import           Godx.Sync.Types

import           Godx.Slotting.Slot (SlotNo (..))

import qualified Godx.Chain.Genesis as Cole
import           Godx.Crypto.ProtocolMagic

import           Data.ByteString (ByteString)
import           Data.Text (Text)

import           Shardagnostic.Consensus.Block.Abstract (HeaderHash, fromRawHash)
import           Shardagnostic.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import           Shardagnostic.Consensus.Node.ProtocolInfo (ProtocolInfo)
import           Shardagnostic.Network.Block (Point (..))
import           Shardagnostic.Network.Magic (NetworkMagic (..))
import qualified Shardagnostic.Network.Point as Point

import qualified Sophie.Spec.Ledger.Genesis as Sophie

data SyncEnv = SyncEnv
  { envProtocol :: !SyncProtocol
  , envNetworkMagic :: !NetworkMagic
  , envSystemStart :: !SystemStart
  , envDataLayer :: !SyncDataLayer
  , envLedger :: !LedgerEnv
  }

-- The base @DataLayer@ that contains the functions required for syncing to work.
data SyncDataLayer = SyncDataLayer
  { sdlGetSlotHash :: SlotNo -> IO [(SlotNo, ByteString)]
  , sdlGetLatestBlock :: IO (Maybe Block)
  , sdlGetLatestSlotNo :: IO SlotNo
  }

mkSyncEnv
    :: SyncDataLayer -> Trace IO Text -> ProtocolInfo IO GodxBlock -> Ledger.Network
    -> NetworkMagic -> SystemStart -> LedgerStateDir -> EpochSlot
    -> IO SyncEnv
mkSyncEnv dataLayer trce protoInfo nw nwMagic systemStart dir stableEpochSlot = do
  ledgerEnv <- mkLedgerEnv trce protoInfo dir nw stableEpochSlot
  pure $ SyncEnv
          { envProtocol = SyncProtocolGodx
          , envNetworkMagic = nwMagic
          , envSystemStart = systemStart
          , envDataLayer = dataLayer
          , envLedger = ledgerEnv
          }

mkSyncEnvFromConfig :: SyncDataLayer -> Trace IO Text -> LedgerStateDir -> GenesisConfig -> IO (Either SyncNodeError SyncEnv)
mkSyncEnvFromConfig trce dataLayer dir genCfg =
    case genCfg of
      GenesisGodx _ bCfg sCfg _aCfg
        | unProtocolMagicId (Cole.configProtocolMagicId bCfg) /= Sophie.sgNetworkMagic (scConfig sCfg) ->
            pure . Left . NEGodxConfig $
              mconcat
                [ "ProtocolMagicId ", textShow (unProtocolMagicId $ Cole.configProtocolMagicId bCfg)
                , " /= ", textShow (Sophie.sgNetworkMagic $ scConfig sCfg)
                ]
        | Cole.gdStartTime (Cole.configGenesisData bCfg) /= Sophie.sgSystemStart (scConfig sCfg) ->
            pure . Left . NEGodxConfig $
              mconcat
                [ "SystemStart ", textShow (Cole.gdStartTime $ Cole.configGenesisData bCfg)
                , " /= ", textShow (Sophie.sgSystemStart $ scConfig sCfg)
                ]
        | otherwise ->
            Right <$> mkSyncEnv trce dataLayer (mkProtocolInfoGodx genCfg) (Sophie.sgNetworkId $ scConfig sCfg)
                        (NetworkMagic . unProtocolMagicId $ Cole.configProtocolMagicId bCfg)
                        (SystemStart .Cole.gdStartTime $ Cole.configGenesisData bCfg)
                        dir (calculateStableEpochSlot $ scConfig sCfg)


getLatestPoints :: SyncEnv -> IO [GodxPoint]
getLatestPoints env = do
    files <- listLedgerStateFilesOrdered $ leDir (envLedger env)
    verifyFilePoints env files

verifyFilePoints :: SyncEnv -> [LedgerStateFile] -> IO [GodxPoint]
verifyFilePoints env files =
    catMaybes <$> mapM validLedgerFileToPoint files
  where
    validLedgerFileToPoint :: LedgerStateFile -> IO (Maybe GodxPoint)
    validLedgerFileToPoint lsf = do
        hashes <- sdlGetSlotHash (envDataLayer env) (lsfSlotNo lsf)
        let valid  = find (\(_, h) -> lsfHash lsf == hashToAnnotation h) hashes
        case valid of
          Just (slot, hash) | slot == lsfSlotNo lsf -> pure $ convert (slot, hash)
          _ -> pure Nothing

    convert :: (SlotNo, ByteString) -> Maybe GodxPoint
    convert (slot, hashBlob) =
      Point . Point.block slot <$> convertHashBlob hashBlob

    convertHashBlob :: ByteString -> Maybe (HeaderHash GodxBlock)
    convertHashBlob = Just . fromRawHash (Proxy @GodxBlock)

-- -------------------------------------------------------------------------------------------------
-- This is incredibly suboptimal. It should work, for now, but may break at some future time and
-- when it is wrong then data in `db-sync` will simply be wrong and we do not have any way of
-- detecting that it is wrong.
--
-- An epoch is `10 k / f` long, and the stability window is `3 k / f` so the time from the start
-- of the epoch to start of the stability window is `7 k / f`.
--
-- Hopefully lower level libraries will be able to provide us with something better than this soon.
calculateStableEpochSlot :: Sophie.SophieGenesis era -> EpochSlot
calculateStableEpochSlot cfg =
    EpochSlot $ ceiling (7.0 * secParam / actSlotCoeff)
  where
    secParam :: Double
    secParam = fromIntegral $ Sophie.sgSecurityParam cfg

    actSlotCoeff :: Double
    actSlotCoeff = fromRational (Ledger.unboundRational $ Sophie.sgActiveSlotsCoeff cfg)
