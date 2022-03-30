{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bcc.Sync.Era.Sophie.Generic.Rewards
  ( Reward (..)
  , Rewards (..)
  , epochRewards
  , rewardsPoolHashKeys
  , rewardsStakeCreds
  ) where

import           Bcc.Prelude

import           Bcc.Db (RewardSource (..), rewardTypeToSource, textShow)

import qualified Bcc.Ledger.Aurum.PParams as Aurum
import qualified Bcc.Ledger.BaseTypes as Ledger
import           Bcc.Ledger.Coin (Coin (..))
import qualified Bcc.Ledger.Core as Ledger
import qualified Bcc.Ledger.Credential as Ledger
import           Bcc.Ledger.Era (Crypto)
import qualified Bcc.Ledger.Keys as Ledger

import           Bcc.Slotting.Slot (EpochNo (..))

import           Bcc.Sync.Era.Sophie.Generic.StakeCred
import           Bcc.Sync.Types

import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Shardagnostic.Consensus.Bcc.Block (LedgerState (..), StandardCrypto)
import           Shardagnostic.Consensus.Bcc.CanHardFork ()
import           Shardagnostic.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Shardagnostic.Consensus.Sophie.Ledger.Block (SophieBlock)
import qualified Shardagnostic.Consensus.Sophie.Ledger.Ledger as Consensus

import qualified Sophie.Spec.Ledger.LedgerState as Sophie
import qualified Sophie.Spec.Ledger.PParams as Sophie
import qualified Sophie.Spec.Ledger.Rewards as Sophie


-- The fields of this struct *must* remain in this ordering in order for the `Ord` instance
-- to work correctly for `takeFirstReward` to operate correctly.
data Reward = Reward
  { rewardSource :: !RewardSource
  , rewardPool :: !(Maybe (Ledger.KeyHash 'Ledger.StakePool StandardCrypto))
  , rewardAmount :: !Coin
  } deriving (Eq, Ord, Show)

-- The `ledger-specs` code defines a `RewardUpdate` type that is parameterised over
-- Sophie/Evie/Jen. This is a huge pain in the neck for `db-sync` so we define a
-- generic one instead.
data Rewards = Rewards
  { rwdEpoch :: !EpochNo
  , rwdRewards :: !(Map StakeCred (Set Reward))
  } deriving Eq

epochRewards :: Ledger.Network -> EpochNo -> ExtLedgerState BccBlock -> Maybe Rewards
epochRewards nw epoch lstate =
    case ledgerState lstate of
      LedgerStateCole _ -> Nothing
      LedgerStateSophie sls -> genericRewards nw era epoch sls
      LedgerStateAllegra als -> genericRewards nw era epoch als
      LedgerStateJen mls -> genericRewards nw era epoch mls
      LedgerStateAurum als -> genericRewards nw era epoch als
  where
    era :: BlockEra
    era = rewardBlockEra $ rewardProtoVer lstate

rewardsPoolHashKeys :: Rewards -> Set PoolKeyHash
rewardsPoolHashKeys rwds =
  Set.fromList . mapMaybe rewardPool
    $ concatMap Set.toList (Map.elems $ rwdRewards rwds)

rewardsStakeCreds :: Rewards -> Set StakeCred
rewardsStakeCreds = Map.keysSet . rwdRewards

rewardBlockEra :: Sophie.ProtVer -> BlockEra
rewardBlockEra pv =
  case pv of
    Sophie.ProtVer 2 0 -> Sophie
    Sophie.ProtVer 3 0 -> Evie
    Sophie.ProtVer 4 0 -> Jen
    Sophie.ProtVer 5 0 -> Aurum
    Sophie.ProtVer 6 0 -> Aurum
    x -> panic $ "rewardBlockEra: " <> textShow x

rewardProtoVer :: ExtLedgerState BccBlock -> Sophie.ProtVer
rewardProtoVer lstate =
    case ledgerState lstate of
      LedgerStateCole _ -> Sophie.ProtVer 1 0 -- Should never happen.
      LedgerStateSophie sls -> Sophie._protocolVersion $ previousPParams sls
      LedgerStateAllegra als -> Sophie._protocolVersion $ previousPParams als
      LedgerStateJen mls -> Sophie._protocolVersion $ previousPParams mls
      LedgerStateAurum als -> Aurum._protocolVersion $ previousPParams als
  where
    -- Get the *previous* block's PParams by using `esPrevPp` `esPp`.
    previousPParams :: LedgerState (SophieBlock era) -> Ledger.PParams era
    previousPParams = Sophie.esPrevPp . Sophie.nesEs . Consensus.sophieLedgerState

-- -------------------------------------------------------------------------------------------------

genericRewards :: forall era. Ledger.Network -> BlockEra -> EpochNo -> LedgerState (SophieBlock era) -> Maybe Rewards
genericRewards network era epoch lstate =
    fmap cleanup rewardUpdate
  where
    cleanup :: Map StakeCred (Set Reward) -> Rewards
    cleanup rmap =
      Rewards
        { rwdEpoch = epoch - 1 -- Epoch in which rewards were earned.
        , rwdRewards = filterByEra era rmap
        }

    rewardUpdate :: Maybe (Map StakeCred (Set Reward))
    rewardUpdate =
      completeRewardUpdate =<< Ledger.strictMaybeToMaybe (Sophie.nesRu $ Consensus.sophieLedgerState lstate)

    completeRewardUpdate :: Sophie.PulsingRewUpdate (Crypto era) -> Maybe (Map StakeCred (Set Reward))
    completeRewardUpdate x =
      case x of
        Sophie.Pulsing {} -> Nothing -- Should never happen.
        Sophie.Complete ru -> Just $ Map.unionWith mappend
                                        (convertRewardMap $ Sophie.rs ru)
                                        (getInstantaneousRewards network lstate)

    convertRewardMap
        :: Map (Ledger.Credential 'Ledger.Staking (Crypto era)) (Set (Sophie.Reward (Crypto era)))
        -> Map StakeCred (Set Reward)
    convertRewardMap = mapBimap (toStakeCred network) (Set.map convertReward)

    convertReward :: Sophie.Reward (Crypto era) -> Reward
    convertReward sr =
      Reward
        { rewardSource = rewardTypeToSource $ Sophie.rewardType sr
        , rewardAmount = Sophie.rewardAmount sr
        , -- Coerce is safe here because we are coercing away an un-needed phantom type parameter (era).
          rewardPool = Just $ coerce (Sophie.rewardPool sr)
        }



mapBimap :: Ord k2 => (k1 -> k2) -> (a1 -> a2) -> Map k1 a1 -> Map k2 a2
mapBimap fk fa = Map.fromAscList . map (bimap fk fa) . Map.toAscList


getInstantaneousRewards :: forall era. Ledger.Network -> LedgerState (SophieBlock era) -> Map StakeCred (Set Reward)
getInstantaneousRewards network lstate =
    Map.unionWith mappend
        (mapBimap (toStakeCred network) (convert RwdReserves) $ Sophie.iRReserves instRwds)
        (mapBimap (toStakeCred network) (convert RwdTreasury) $ Sophie.iRTreasury instRwds)
  where
    convert :: RewardSource -> Coin -> Set Reward
    convert rs coin =
      Set.singleton
        Reward
          { rewardSource = rs
          , rewardAmount = coin
          , rewardPool = Nothing
          }

    instRwds :: Sophie.InstantaneousRewards (Crypto era)
    instRwds =
      Sophie._irwd . Sophie._dstate . Sophie._delegationState
        . Sophie.esLState . Sophie.nesEs $ Consensus.sophieLedgerState lstate

-- -------------------------------------------------------------------------------------------------
-- `db-sync` needs to match the implementation of the logic in `ledger-specs` even when that logic
-- is not actually correct (ie it needs to be bug compatible). Eg it was intended that a single
-- stake address can receive rewards from more than one place (eg for being a pool owner and a
-- pool member or rewards from two separate pools going to the name stake address). However, due
-- to a bug in `ledger-specs` all rewards other than the first are accidentally dropped (caused by
-- the use of `Map.union` instead of `Map.unionWith mapppend`). These missing rewards have since
-- been paid back with payments from the reserves.

filterByEra :: BlockEra -> Map StakeCred (Set Reward) -> Map StakeCred (Set Reward)
filterByEra be rmap =
  case be of
    Sophie -> Map.map takeFirstReward rmap
    Evie -> rmap
    Jen -> rmap
    Aurum -> rmap

-- This emulates the `ledger-specs` bug by taking the first element of the reward Set.
-- The `Ord` instance on `Reward`, orders by `rewardSource` first, then `rewardPool` and then
-- `rewardAmount`.
takeFirstReward :: Set Reward -> Set Reward
takeFirstReward rs =
  -- The `toList` operation returns an ordered list.
  case Set.toList rs of
    [] -> mempty -- Should never happen.
    x:_ -> Set.singleton x
