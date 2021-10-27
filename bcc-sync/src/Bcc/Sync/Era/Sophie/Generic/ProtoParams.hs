{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Godx.Sync.Era.Sophie.Generic.ProtoParams
  ( ProtoParams (..)
  , epochProtoParams
  ) where

import           Godx.Prelude

import qualified Godx.Ledger.Aurum as Aurum
import           Godx.Ledger.Aurum.Language (Language)
import qualified Godx.Ledger.Aurum.PParams as Aurum
import qualified Godx.Ledger.Aurum.Scripts as Aurum
import           Godx.Ledger.BaseTypes (UnitInterval)
import qualified Godx.Ledger.BaseTypes as Ledger
import           Godx.Ledger.Coin (Coin (..))
import           Godx.Slotting.Slot (EpochNo (..))

import           Godx.Sync.Types

import           Shardagnostic.Consensus.Godx.Block (LedgerState (..), StandardAllegra,
                   StandardAurum, StandardCrypto, StandardJen, StandardSophie)

import           Shardagnostic.Consensus.Godx (Nonce (..))
import           Shardagnostic.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Shardagnostic.Consensus.Sophie.Ledger.Block (SophieBlock)
import qualified Shardagnostic.Consensus.Sophie.Ledger.Ledger as Consensus

import qualified Sophie.Spec.Ledger.LedgerState as Sophie
import           Sophie.Spec.Ledger.PParams (ProtVer)
import qualified Sophie.Spec.Ledger.PParams as Sophie

data ProtoParams = ProtoParams
  { ppMinfeeA :: !Natural
  , ppMinfeeB :: !Natural
  , ppMaxBBSize :: !Natural
  , ppMaxTxSize :: !Natural
  , ppMaxBHSize :: !Natural
  , ppKeyDeposit :: !Coin
  , ppPoolDeposit :: !Coin
  , ppMaxEpoch :: !EpochNo
  , ppOptialPoolCount :: !Natural
  , ppInfluence :: !Rational
  , ppMonetaryExpandRate :: !UnitInterval
  , ppTreasuryGrowthRate :: !UnitInterval
  , ppDecentralisation :: !UnitInterval
  , ppExtraEntropy :: !Nonce
  , ppProtocolVersion :: !ProtVer
  , ppMinUTxOValue :: !Coin
  , ppMinPoolCost :: !Coin

  -- New for Aurum.
  , ppCoinsPerUtxoWord :: !(Maybe Coin)
  , ppCostmdls :: !(Maybe (Map Language Aurum.CostModel))
  , ppPriceMem :: !(Maybe Rational)
  , ppPriceStep :: !(Maybe Rational)
  , ppMaxTxExMem :: !(Maybe Word64)
  , ppMaxTxExSteps :: !(Maybe Word64)
  , ppMaxBlockExMem :: !(Maybe Word64)
  , ppMaxBlockExSteps :: !(Maybe Word64)
  , ppMaxValSize :: !(Maybe Natural)
  , ppCollateralPercentage :: !(Maybe Natural)
  , ppMaxCollateralInputs :: !(Maybe Natural)
  }

epochProtoParams :: ExtLedgerState GodxBlock -> Maybe ProtoParams
epochProtoParams lstate =
    case ledgerState lstate of
      LedgerStateCole _ -> Nothing
      LedgerStateSophie sls -> Just $ sophieProtoParams sls
      LedgerStateAllegra als -> Just $ evieProtoParams als
      LedgerStateJen mls -> Just $ maryProtoParams mls
      LedgerStateAurum als -> Just $ aurumProtoParams als

evieProtoParams :: LedgerState (SophieBlock StandardAllegra) -> ProtoParams
evieProtoParams =
  fromSophieParams . Sophie.esPp . Sophie.nesEs . Consensus.sophieLedgerState

aurumProtoParams :: LedgerState (SophieBlock StandardAurum) -> ProtoParams
aurumProtoParams =
  fromAurumParams . Sophie.esPp . Sophie.nesEs . Consensus.sophieLedgerState

maryProtoParams :: LedgerState (SophieBlock StandardJen) -> ProtoParams
maryProtoParams =
  fromSophieParams . Sophie.esPp . Sophie.nesEs . Consensus.sophieLedgerState

sophieProtoParams :: LedgerState (SophieBlock StandardSophie) -> ProtoParams
sophieProtoParams =
  fromSophieParams . Sophie.esPp . Sophie.nesEs . Consensus.sophieLedgerState

-- -------------------------------------------------------------------------------------------------

fromAurumParams :: Aurum.PParams (Aurum.AurumEra StandardCrypto) -> ProtoParams
fromAurumParams params =
  ProtoParams
    { ppMinfeeA = Aurum._minfeeA params
    , ppMinfeeB = Aurum._minfeeB params
    , ppMaxBBSize = Aurum._maxBBSize params
    , ppMaxTxSize = Aurum._maxTxSize params
    , ppMaxBHSize = Aurum._maxBHSize params
    , ppKeyDeposit = Aurum._keyDeposit params
    , ppPoolDeposit = Aurum._poolDeposit params
    , ppMaxEpoch = Aurum._eMax params
    , ppOptialPoolCount = Aurum._nOpt params
    , ppInfluence = Ledger.unboundRational $ Aurum._a0 params
    , ppMonetaryExpandRate = Aurum._rho params
    , ppTreasuryGrowthRate = Aurum._tau params
    , ppDecentralisation  = Aurum._d params
    , ppExtraEntropy = Aurum._extraEntropy params
    , ppProtocolVersion = Aurum._protocolVersion params
    , ppMinUTxOValue = Aurum._coinsPerUTxOWord params
    , ppMinPoolCost = Aurum._minPoolCost params
    , ppCoinsPerUtxoWord = Just $ Aurum._coinsPerUTxOWord params
    , ppCostmdls = Just $ Aurum._costmdls params
    , ppPriceMem = Just . Ledger.unboundRational $ Aurum.prMem (Aurum._prices params)
    , ppPriceStep = Just . Ledger.unboundRational $ Aurum.prSteps (Aurum._prices params)
    , ppMaxTxExMem = Just $ Aurum.exUnitsMem (Aurum._maxTxExUnits params)
    , ppMaxTxExSteps = Just $ Aurum.exUnitsSteps (Aurum._maxTxExUnits params)
    , ppMaxBlockExMem = Just $ Aurum.exUnitsMem (Aurum._maxBlockExUnits params)
    , ppMaxBlockExSteps = Just $ Aurum.exUnitsSteps (Aurum._maxBlockExUnits params)
    , ppMaxValSize = Just $ Aurum._maxValSize params
    , ppCollateralPercentage = Just $ Aurum._collateralPercentage params
    , ppMaxCollateralInputs = Just $ Aurum._maxCollateralInputs params
    }

fromSophieParams :: Sophie.PParams' Identity era -> ProtoParams
fromSophieParams params =
  ProtoParams
    { ppMinfeeA = Sophie._minfeeA params
    , ppMinfeeB = Sophie._minfeeB params
    , ppMaxBBSize = Sophie._maxBBSize params
    , ppMaxTxSize = Sophie._maxTxSize params
    , ppMaxBHSize = Sophie._maxBHSize params
    , ppKeyDeposit = Sophie._keyDeposit params
    , ppPoolDeposit = Sophie._poolDeposit params
    , ppMaxEpoch = Sophie._eMax params
    , ppOptialPoolCount = Sophie._nOpt params
    , ppInfluence = Ledger.unboundRational $ Sophie._a0 params
    , ppMonetaryExpandRate = Sophie._rho params
    , ppTreasuryGrowthRate = Sophie._tau params
    , ppDecentralisation  = Sophie._d params
    , ppExtraEntropy = Sophie._extraEntropy params
    , ppProtocolVersion = Sophie._protocolVersion params
    , ppMinUTxOValue = Sophie._minUTxOValue params
    , ppMinPoolCost = Sophie._minPoolCost params
    , ppCoinsPerUtxoWord = Nothing
    , ppCostmdls = Nothing
    , ppPriceMem = Nothing
    , ppPriceStep = Nothing
    , ppMaxTxExMem = Nothing
    , ppMaxTxExSteps = Nothing
    , ppMaxBlockExMem = Nothing
    , ppMaxBlockExSteps = Nothing
    , ppMaxValSize = Nothing
    , ppCollateralPercentage = Nothing
    , ppMaxCollateralInputs = Nothing
    }
