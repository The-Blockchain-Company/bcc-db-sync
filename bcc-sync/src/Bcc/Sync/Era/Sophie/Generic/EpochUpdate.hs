{-# LANGUAGE NoImplicitPrelude #-}
module Godx.Sync.Era.Sophie.Generic.EpochUpdate
  ( NewEpoch (..)
  , EpochUpdate (..)
  , epochUpdate
  ) where

import           Godx.Prelude hiding (Maybe (..), fromMaybe)

import           Godx.Slotting.Slot (EpochNo (..))

import qualified Godx.Ledger.BaseTypes as Ledger

import           Godx.Sync.Era.Sophie.Generic.ProtoParams
import           Godx.Sync.Types
import           Godx.Sync.Util

import           Data.Strict.Maybe (Maybe (..))

import           Shardagnostic.Consensus.Godx.Block (HardForkState (..))
import           Shardagnostic.Consensus.Godx.CanHardFork ()
import qualified Shardagnostic.Consensus.HeaderValidation as Consensus
import           Shardagnostic.Consensus.Ledger.Extended (ExtLedgerState (..))
import qualified Shardagnostic.Consensus.Sophie.Protocol as Consensus

import qualified Sophie.Spec.Ledger.API.Protocol as Sophie
import qualified Sophie.Spec.Ledger.STS.Chain as Sophie
import qualified Sophie.Spec.Ledger.STS.Tickn as Sophie

data NewEpoch = NewEpoch
  { neEpoch :: !EpochNo
  , neIsEBB :: !Bool
  , neGodxPots :: !(Maybe Sophie.GodxPots)
  , neEpochUpdate :: !EpochUpdate
  }

data EpochUpdate = EpochUpdate
  { euProtoParams :: !(Maybe ProtoParams)
  , euNonce :: !Ledger.Nonce
  }

epochUpdate :: ExtLedgerState GodxBlock -> EpochUpdate
epochUpdate lstate =
  EpochUpdate
    { euProtoParams = maybeToStrict $ epochProtoParams lstate
    , euNonce = extractEpochNonce lstate
    }

-- -------------------------------------------------------------------------------------------------

extractEpochNonce :: ExtLedgerState GodxBlock -> Ledger.Nonce
extractEpochNonce extLedgerState =
    case Consensus.headerStateChainDep (headerState extLedgerState) of
      ChainDepStateCole _ -> Ledger.NeutralNonce
      ChainDepStateSophie st -> extractNonce st
      ChainDepStateAllegra st -> extractNonce st
      ChainDepStateJen st -> extractNonce st
      ChainDepStateAurum st -> extractNonce st
  where
    extractNonce :: Consensus.TOptimumState crypto -> Ledger.Nonce
    extractNonce =
      Sophie.ticknStateEpochNonce . Sophie.csTickn . Consensus.toptimumStateChainDepState

