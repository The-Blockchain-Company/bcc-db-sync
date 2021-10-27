{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Godx.DbSync.Era.Sophie.Generic.ParamProposal
  ( ParamProposal (..)
  , convertParamProposal
  ) where

import           Godx.Prelude

import           Godx.DbSync.Era.Sophie.Generic.Util (unKeyHashRaw)
import           Godx.DbSync.Era.Sophie.Generic.Witness (Witness (..))

import qualified Godx.Ledger.Aurum as Aurum
import           Godx.Ledger.Aurum.Language (Language)
import qualified Godx.Ledger.Aurum.PParams as Aurum
import qualified Godx.Ledger.Aurum.Scripts as Aurum
import           Godx.Ledger.BaseTypes (UnitInterval, strictMaybeToMaybe)
import qualified Godx.Ledger.BaseTypes as Ledger
import           Godx.Ledger.Coin (Coin)
import qualified Godx.Ledger.Keys as Ledger
import           Godx.Ledger.Sophie (SophieEra)
import qualified Godx.Ledger.SophieMA as SophieMA

import           Godx.Slotting.Slot (EpochNo (..))

import qualified Data.Map.Strict as Map

import qualified Sophie.Spec.Ledger.PParams as Sophie

data ParamProposal = ParamProposal
  { pppEpochNo :: !EpochNo
  , pppKey :: !ByteString
  , pppMinFeeA :: !(Maybe Natural)
  , pppMinFeeB :: !(Maybe Natural)
  , pppMaxBlockSize :: !(Maybe Natural)
  , pppMaxTxSize :: !(Maybe Natural)
  , pppMaxBhSize :: !(Maybe Natural)
  , pppKeyDeposit :: !(Maybe Coin)
  , pppPoolDeposit :: !(Maybe Coin)
  , pppMaxEpoch :: !(Maybe EpochNo)
  , pppOptimalPoolCount :: !(Maybe Natural)
  , pppInfluence :: !(Maybe Rational)
  , pppMonetaryExpandRate :: !(Maybe UnitInterval)
  , pppTreasuryGrowthRate :: !(Maybe UnitInterval)
  , pppDecentralisation :: !(Maybe UnitInterval)
  , pppEntropy :: !(Maybe Ledger.Nonce)
  , pppProtocolVersion :: !(Maybe Sophie.ProtVer)
  , pppMinUtxoValue :: !(Maybe Coin)
  , pppMinPoolCost :: !(Maybe Coin)

  -- New for Aurum.
  , pppCoinsPerUtxoWord :: !(Maybe Coin)
  , pppCostmdls :: !(Maybe (Map Language Aurum.CostModel))
  , pppPriceMem :: !(Maybe Rational)
  , pppPriceStep :: !(Maybe Rational)
  , pppMaxTxExMem :: !(Maybe Word64)
  , pppMaxTxExSteps :: !(Maybe Word64)
  , pppMaxBlockExMem :: !(Maybe Word64)
  , pppMaxBlockExSteps :: !(Maybe Word64)
  , pppMaxValSize :: !(Maybe Natural)
  , pppCollateralPercentage :: !(Maybe Natural)
  , pppMaxCollateralInputs :: !(Maybe Natural)
  }

convertParamProposal :: Witness era -> Sophie.Update era -> [ParamProposal]
convertParamProposal witness (Sophie.Update pp epoch) =
  case witness of
    Sophie {} -> sophieParamProposal epoch pp
    Allegra {} -> evieOrJenParamProposal epoch pp
    Jen {} -> evieOrJenParamProposal epoch pp
    Aurum {} -> aurumParamProposal epoch pp

-- -------------------------------------------------------------------------------------------------

evieOrJenParamProposal :: EpochNo -> Sophie.ProposedPPUpdates (SophieMA.SophieMAEra a c) -> [ParamProposal]
evieOrJenParamProposal epochNo (Sophie.ProposedPPUpdates umap) =
    map (convertSophieParamProposal epochNo) $ Map.toList umap

aurumParamProposal :: EpochNo -> Sophie.ProposedPPUpdates (Aurum.AurumEra c) -> [ParamProposal]
aurumParamProposal epochNo (Sophie.ProposedPPUpdates umap) =
    map (convertAurumParamProposal epochNo) $ Map.toList umap

sophieParamProposal :: EpochNo -> Sophie.ProposedPPUpdates (SophieEra c) -> [ParamProposal]
sophieParamProposal epochNo (Sophie.ProposedPPUpdates umap) =
    map (convertSophieParamProposal epochNo) $ Map.toList umap

-- -------------------------------------------------------------------------------------------------

convertAurumParamProposal :: EpochNo -> (Ledger.KeyHash genesis crypto, Aurum.PParamsUpdate era) -> ParamProposal
convertAurumParamProposal  epochNo (key, pmap) =
  ParamProposal
    { pppEpochNo = epochNo
    , pppKey = unKeyHashRaw key
    , pppMinFeeA = strictMaybeToMaybe (Aurum._minfeeA pmap)
    , pppMinFeeB = strictMaybeToMaybe (Aurum._minfeeB pmap)
    , pppMaxBlockSize = strictMaybeToMaybe (Aurum._maxBBSize pmap)
    , pppMaxTxSize = strictMaybeToMaybe (Aurum._maxTxSize pmap)
    , pppMaxBhSize = strictMaybeToMaybe (Aurum._maxBHSize pmap)
    , pppKeyDeposit = strictMaybeToMaybe (Aurum._keyDeposit pmap)
    , pppPoolDeposit = strictMaybeToMaybe (Aurum._poolDeposit pmap)
    , pppMaxEpoch = strictMaybeToMaybe (Aurum._eMax pmap)
    , pppOptimalPoolCount = strictMaybeToMaybe (Aurum._nOpt pmap)
    , pppInfluence = Ledger.unboundRational <$> strictMaybeToMaybe (Aurum._a0 pmap)
    , pppMonetaryExpandRate = strictMaybeToMaybe (Aurum._rho pmap)
    , pppTreasuryGrowthRate = strictMaybeToMaybe (Aurum._tau pmap)
    , pppDecentralisation = strictMaybeToMaybe (Aurum._d pmap)
    , pppEntropy = strictMaybeToMaybe (Aurum._extraEntropy pmap)
    , pppProtocolVersion = strictMaybeToMaybe (Aurum._protocolVersion pmap)
    , pppMinUtxoValue = Nothing -- Removed in Aurum
    , pppMinPoolCost = strictMaybeToMaybe (Aurum._minPoolCost pmap)

    -- New for Aurum.
    , pppCoinsPerUtxoWord = strictMaybeToMaybe (Aurum._coinsPerUTxOWord pmap)
    , pppCostmdls = strictMaybeToMaybe (Aurum._costmdls pmap)
    , pppPriceMem = Ledger.unboundRational . Aurum.prMem <$> strictMaybeToMaybe (Aurum._prices pmap)
    , pppPriceStep = Ledger.unboundRational . Aurum.prSteps <$> strictMaybeToMaybe (Aurum._prices pmap)
    , pppMaxTxExMem = Aurum.exUnitsMem <$> strictMaybeToMaybe (Aurum._maxTxExUnits pmap)
    , pppMaxTxExSteps = Aurum.exUnitsSteps <$> strictMaybeToMaybe (Aurum._maxTxExUnits pmap)
    , pppMaxBlockExMem = Aurum.exUnitsMem <$> strictMaybeToMaybe (Aurum._maxBlockExUnits pmap)
    , pppMaxBlockExSteps = Aurum.exUnitsSteps <$> strictMaybeToMaybe (Aurum._maxBlockExUnits pmap)
    , pppMaxValSize = strictMaybeToMaybe (Aurum._maxValSize pmap)
    , pppCollateralPercentage = strictMaybeToMaybe (Aurum._collateralPercentage pmap)
    , pppMaxCollateralInputs = strictMaybeToMaybe (Aurum._maxCollateralInputs pmap)
    }

convertSophieParamProposal :: EpochNo -> (Ledger.KeyHash genesis crypto, Sophie.PParams' Ledger.StrictMaybe era) -> ParamProposal
convertSophieParamProposal epochNo (key, pmap) =
  ParamProposal
    { pppEpochNo = epochNo
    , pppKey = unKeyHashRaw key
    , pppMinFeeA = strictMaybeToMaybe (Sophie._minfeeA pmap)
    , pppMinFeeB = strictMaybeToMaybe (Sophie._minfeeB pmap)
    , pppMaxBlockSize = strictMaybeToMaybe (Sophie._maxBBSize pmap)
    , pppMaxTxSize = strictMaybeToMaybe (Sophie._maxTxSize pmap)
    , pppMaxBhSize = strictMaybeToMaybe (Sophie._maxBHSize pmap)
    , pppKeyDeposit = strictMaybeToMaybe (Sophie._keyDeposit pmap)
    , pppPoolDeposit = strictMaybeToMaybe (Sophie._poolDeposit pmap)
    , pppMaxEpoch = strictMaybeToMaybe (Sophie._eMax pmap)
    , pppOptimalPoolCount = strictMaybeToMaybe (Sophie._nOpt pmap)
    , pppInfluence = Ledger.unboundRational <$> strictMaybeToMaybe (Sophie._a0 pmap)
    , pppMonetaryExpandRate = strictMaybeToMaybe (Sophie._rho pmap)
    , pppTreasuryGrowthRate = strictMaybeToMaybe (Sophie._tau pmap)
    , pppDecentralisation = strictMaybeToMaybe (Sophie._d pmap)
    , pppEntropy = strictMaybeToMaybe (Sophie._extraEntropy pmap)
    , pppProtocolVersion = strictMaybeToMaybe (Sophie._protocolVersion pmap)
    , pppMinUtxoValue = strictMaybeToMaybe (Sophie._minUTxOValue pmap)
    , pppMinPoolCost = strictMaybeToMaybe (Sophie._minPoolCost pmap)

    -- The following are Aurum related, hence Nothing.
    , pppCoinsPerUtxoWord = Nothing
    , pppCostmdls = Nothing
    , pppPriceMem = Nothing
    , pppPriceStep = Nothing
    , pppMaxTxExMem = Nothing
    , pppMaxTxExSteps = Nothing
    , pppMaxBlockExMem = Nothing
    , pppMaxBlockExSteps = Nothing
    , pppMaxValSize = Nothing
    , pppCollateralPercentage = Nothing
    , pppMaxCollateralInputs = Nothing
    }
