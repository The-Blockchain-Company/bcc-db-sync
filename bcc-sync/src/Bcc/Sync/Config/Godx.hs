{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Godx.Sync.Config.Godx
  ( GenesisConfig (..)
  , bccLedgerConfig
  , genesisProtocolMagicId
  , mkTopLevelConfig
  , mkProtocolInfoGodx
  , readGodxGenesisConfig
  ) where

import qualified Godx.Chain.Genesis as Cole
import qualified Godx.Chain.Update as Cole
import           Godx.Crypto.ProtocolMagic (ProtocolMagicId (..))

import qualified Godx.Crypto.Hash.Class as Crypto

import           Godx.Ledger.Aurum.Genesis

import           Godx.Sync.Config.Aurum
import           Godx.Sync.Config.Cole
import           Godx.Sync.Config.Sophie
import           Godx.Sync.Config.Types
import           Godx.Sync.Error

import           Control.Monad.Trans.Except (ExceptT)

import           Shardagnostic.Consensus.Godx (Nonce (..))
import qualified Shardagnostic.Consensus.Godx as Consensus
import qualified Shardagnostic.Consensus.Godx.Node as Consensus
import           Shardagnostic.Consensus.Config (TopLevelConfig (..))
import           Shardagnostic.Consensus.Ledger.Basics (LedgerConfig)
import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits
import           Shardagnostic.Consensus.Node.ProtocolInfo (ProtocolInfo)
import qualified Shardagnostic.Consensus.Node.ProtocolInfo as Consensus
import           Shardagnostic.Consensus.Sophie.Eras (StandardSophie)
import           Shardagnostic.Consensus.Sophie.Node (SophieGenesis (..))

import qualified Sophie.Spec.Ledger.PParams as Sophie


-- Usually only one constructor, but may have two when we are preparing for a HFC event.
data GenesisConfig
  = GenesisGodx !SyncNodeConfig !Cole.Config !SophieConfig !AurumGenesis

genesisProtocolMagicId :: GenesisConfig -> ProtocolMagicId
genesisProtocolMagicId ge =
    case ge of
      GenesisGodx _cfg _bCfg sCfg _aCfg -> sophieProtocolMagicId (scConfig sCfg)
  where
    sophieProtocolMagicId :: SophieGenesis StandardSophie -> ProtocolMagicId
    sophieProtocolMagicId sCfg = ProtocolMagicId (sgNetworkMagic sCfg)

readGodxGenesisConfig
        :: SyncNodeConfig
        -> ExceptT SyncNodeError IO GenesisConfig
readGodxGenesisConfig enc =
  case dncProtocol enc of
    SyncProtocolGodx ->
      GenesisGodx enc <$> readColeGenesisConfig enc
                         <*> readSophieGenesisConfig enc
                         <*> readAurumGenesisConfig enc

-- -------------------------------------------------------------------------------------------------

bccLedgerConfig :: GenesisConfig -> LedgerConfig GodxBlock
bccLedgerConfig = topLevelConfigLedger . mkTopLevelConfig

mkTopLevelConfig :: GenesisConfig -> TopLevelConfig GodxBlock
mkTopLevelConfig = Consensus.pInfoConfig . mkProtocolInfoGodx

-- Need a concrete type for 'm' ('IO') to make the type checker happy.
-- | The vast majority of the following struct fields are *COMPLETELY IRRELEVANT* to the
-- operation of db-sync, but I have no idea at all what happens of any of these are
-- wrong. This really needs to be a done a different way.
-- mkProtocolGodx :: GenesisConfig -> Protocol m GodxBlock GodxProtocol
mkProtocolInfoGodx :: GenesisConfig -> ProtocolInfo IO GodxBlock
mkProtocolInfoGodx ge =
  case ge of
    GenesisGodx dnc coleGenesis sophieGenesis aurumGenesis ->
        Consensus.protocolInfoGodx
          Consensus.ProtocolParamsCole
            { Consensus.coleGenesis = coleGenesis
            , Consensus.colePbftSignatureThreshold = Consensus.PBftSignatureThreshold <$> dncPBftSignatureThreshold dnc
            , Consensus.coleProtocolVersion = dncColeProtocolVersion dnc
            , Consensus.coleSoftwareVersion = dncColeSoftwareVersion dnc
            , Consensus.coleLeaderCredentials = Nothing
            , Consensus.coleMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          Consensus.ProtocolParamsSophieBased
            { Consensus.sophieBasedGenesis = scConfig sophieGenesis
            , Consensus.sophieBasedInitialNonce = sophiePraosNonce sophieGenesis
            , Consensus.sophieBasedLeaderCredentials = []
            }
          Consensus.ProtocolParamsSophie
            { Consensus.sophieProtVer = sophieProtVer dnc
            , Consensus.sophieMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          Consensus.ProtocolParamsAllegra
            { Consensus.evieProtVer = sophieProtVer dnc
            , Consensus.evieMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          Consensus.ProtocolParamsJen
            { Consensus.maryProtVer = sophieProtVer dnc
            , Consensus.maryMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          Consensus.ProtocolParamsAurum
            { Consensus.aurumProtVer = sophieProtVer dnc
            , Consensus.aurumMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          (Consensus.ProtocolTransitionParamsSophieBased () $ dncSophieHardFork dnc)
          (Consensus.ProtocolTransitionParamsSophieBased () $ dncAllegraHardFork dnc)
          (Consensus.ProtocolTransitionParamsSophieBased () $ dncJenHardFork dnc)
          (Consensus.ProtocolTransitionParamsSophieBased aurumGenesis $ dncAurumHardFork dnc)

sophiePraosNonce :: SophieConfig -> Nonce
sophiePraosNonce sCfg = Nonce (Crypto.castHash . unGenesisHashSophie $ scGenesisHash sCfg)

sophieProtVer :: SyncNodeConfig -> Sophie.ProtVer
sophieProtVer dnc =
  let bver = dncColeProtocolVersion dnc in
  Sophie.ProtVer (fromIntegral $ Cole.pvMajor bver) (fromIntegral $ Cole.pvMinor bver)

