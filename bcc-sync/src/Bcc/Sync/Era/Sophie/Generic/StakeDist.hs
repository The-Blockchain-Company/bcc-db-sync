{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bcc.Sync.Era.Sophie.Generic.StakeDist
  ( StakeDist (..)
  , epochStakeDist
  , stakeDistPoolHashKeys
  , stakeDistStakeCreds
  ) where

import           Bcc.Prelude

import qualified Bcc.Ledger.BaseTypes as Ledger
import           Bcc.Ledger.Coin (Coin (..))
import           Bcc.Ledger.Credential (Credential)
import           Bcc.Ledger.Era (Crypto)
import           Bcc.Ledger.Keys (KeyHash, KeyRole (..))

import           Bcc.Slotting.Slot (EpochNo (..))

import           Bcc.Sync.Era.Sophie.Generic.StakeCred
import           Bcc.Sync.Types

import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Shardagnostic.Consensus.Bcc.Block (LedgerState (..), StandardCrypto)

import           Shardagnostic.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Shardagnostic.Consensus.Sophie.Ledger.Block (SophieBlock)
import qualified Shardagnostic.Consensus.Sophie.Ledger.Ledger as Consensus

import qualified Sophie.Spec.Ledger.EpochBoundary as Sophie
import qualified Sophie.Spec.Ledger.LedgerState as Sophie hiding (_delegations)


data StakeDist = StakeDist
  { sdistEpochNo :: !EpochNo
  , sdistStakeMap :: !(Map StakeCred (Coin, KeyHash 'StakePool StandardCrypto))
  } deriving Eq

epochStakeDist :: Ledger.Network -> EpochNo -> ExtLedgerState BccBlock -> Maybe StakeDist
epochStakeDist network epoch els =
  case ledgerState els of
    LedgerStateCole _ -> Nothing
    LedgerStateSophie sls -> Just $ genericStakeDist network epoch sls
    LedgerStateAllegra als -> Just $ genericStakeDist network epoch als
    LedgerStateJen mls -> Just $ genericStakeDist network epoch mls
    LedgerStateAurum als -> Just $ genericStakeDist network epoch als

-- Use Set because they guarantee unique elements.
stakeDistPoolHashKeys :: StakeDist -> Set PoolKeyHash
stakeDistPoolHashKeys = Set.fromList . map snd . Map.elems . sdistStakeMap

stakeDistStakeCreds :: StakeDist -> Set StakeCred
stakeDistStakeCreds = Map.keysSet . sdistStakeMap

-- -------------------------------------------------------------------------------------------------

genericStakeDist :: forall era. Ledger.Network -> EpochNo -> LedgerState (SophieBlock era) -> StakeDist
genericStakeDist network epoch lstate =
    StakeDist
      { sdistEpochNo = epoch
      , sdistStakeMap = stakeMap
      }
  where
    stakeMap :: Map StakeCred (Coin, KeyHash 'StakePool StandardCrypto)
    stakeMap =
      Map.mapKeys (toStakeCred network) $
        Map.intersectionWith (,) stakeCoinMap stakePoolMap

    -- We used coerce here on a phanton type: Crypto era -> StandardCrypto.
    stakeCoinMap :: Map (Credential 'Staking StandardCrypto) Coin
    stakeCoinMap = Map.mapKeys coerce . Sophie.unStake $ Sophie._stake stakeSet

    stakePoolMap :: Map (Credential 'Staking StandardCrypto) (KeyHash 'StakePool StandardCrypto)
    stakePoolMap = mapBimap coerce coerce $ Sophie._delegations stakeSet

    -- We use '_pstakeSet' here instead of '_pstateMark' because the stake addresses for the
    -- later may not have been added to the database yet. That means that when these values
    -- are added to the database, the epoch number where they become active is the current
    -- epoch plus one.

    stakeSet :: Sophie.SnapShot (Crypto era)
    stakeSet = Sophie._pstakeSet . Sophie.esSnapshots . Sophie.nesEs
                $ Consensus.sophieLedgerState lstate

-- Is there a better way to do this?
mapBimap :: Ord k2 => (k1 -> k2) -> (a1 -> a2) -> Map k1 a1 -> Map k2 a2
mapBimap fk fa = Map.fromAscList . map (bimap fk fa) . Map.toAscList
