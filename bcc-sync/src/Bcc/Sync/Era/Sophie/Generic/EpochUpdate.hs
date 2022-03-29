{-# LANGUAGE NoImplicitPrelude #-}
module Bcc.Sync.Era.Sophie.Generic.EpochUpdate
  ( NewEpoch (..)
  , EpochUpdate (..)
  , epochUpdate
  ) where

import           Bcc.Prelude hiding (Maybe (..), fromMaybe)

import           Bcc.Slotting.Slot (EpochNo (..))

import qualified Bcc.Ledger.BaseTypes as Ledger

import           Bcc.Sync.Era.Sophie.Generic.ProtoParams
import           Bcc.Sync.Types
import           Bcc.Sync.Util

import           Data.Strict.Maybe (Maybe (..))

import           Shardagnostic.Consensus.Bcc.Block (HardForkState (..))
import           Shardagnostic.Consensus.Bcc.CanHardFork ()
import qualified Shardagnostic.Consensus.HeaderValidation as Consensus
import           Shardagnostic.Consensus.Ledger.Extended (ExtLedgerState (..))
import qualified Shardagnostic.Consensus.Sophie.Protocol as Consensus

import qualified Sophie.Spec.Ledger.API.Protocol as Sophie
import qualified Sophie.Spec.Ledger.STS.Chain as Sophie
import qualified Sophie.Spec.Ledger.STS.Tickn as Sophie

data NewEpoch = NewEpoch
  { neEpoch :: !EpochNo
  , neIsEBB :: !Bool
  , neBccPots :: !(Maybe Sophie.BccPots)
  , neEpochUpdate :: !EpochUpdate
  }

data EpochUpdate = EpochUpdate
  { euProtoParams :: !(Maybe ProtoParams)
  , euNonce :: !Ledger.Nonce
  }

epochUpdate :: ExtLedgerState BccBlock -> EpochUpdate
epochUpdate lstate =
  EpochUpdate
    { euProtoParams = maybeToStrict $ epochProtoParams lstate
    , euNonce = extractEpochNonce lstate
    }

-- -------------------------------------------------------------------------------------------------

extractEpochNonce :: ExtLedgerState BccBlock -> Ledger.Nonce
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

