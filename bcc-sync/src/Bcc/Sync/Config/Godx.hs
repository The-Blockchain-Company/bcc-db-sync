{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Bcc.Sync.Config.Bcc
  ( GenesisConfig (..)
  , bccLedgerConfig
  , genesisProtocolMagicId
  , mkTopLevelConfig
  , mkProtocolInfoBcc
  , readBccGenesisConfig
  ) where

import qualified Bcc.Chain.Genesis as Cole
import qualified Bcc.Chain.Update as Cole
import           Bcc.Crypto.ProtocolMagic (ProtocolMagicId (..))

import qualified Bcc.Crypto.Hash.Class as Crypto

import           Bcc.Ledger.Aurum.Genesis

import           Bcc.Sync.Config.Aurum
import           Bcc.Sync.Config.Cole
import           Bcc.Sync.Config.Sophie
import           Bcc.Sync.Config.Types
import           Bcc.Sync.Error

import           Control.Monad.Trans.Except (ExceptT)

import           Shardagnostic.Consensus.Bcc (Nonce (..))
import qualified Shardagnostic.Consensus.Bcc as Consensus
import qualified Shardagnostic.Consensus.Bcc.Node as Consensus
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
  = GenesisBcc !SyncNodeConfig !Cole.Config !SophieConfig !AurumGenesis

genesisProtocolMagicId :: GenesisConfig -> ProtocolMagicId
genesisProtocolMagicId ge =
    case ge of
      GenesisBcc _cfg _bCfg sCfg _aCfg -> sophieProtocolMagicId (scConfig sCfg)
  where
    sophieProtocolMagicId :: SophieGenesis StandardSophie -> ProtocolMagicId
    sophieProtocolMagicId sCfg = ProtocolMagicId (sgNetworkMagic sCfg)

readBccGenesisConfig
        :: SyncNodeConfig
        -> ExceptT SyncNodeError IO GenesisConfig
readBccGenesisConfig enc =
  case dncProtocol enc of
    SyncProtocolBcc ->
      GenesisBcc enc <$> readColeGenesisConfig enc
                         <*> readSophieGenesisConfig enc
                         <*> readAurumGenesisConfig enc

-- -------------------------------------------------------------------------------------------------

bccLedgerConfig :: GenesisConfig -> LedgerConfig BccBlock
bccLedgerConfig = topLevelConfigLedger . mkTopLevelConfig

mkTopLevelConfig :: GenesisConfig -> TopLevelConfig BccBlock
mkTopLevelConfig = Consensus.pInfoConfig . mkProtocolInfoBcc

-- Need a concrete type for 'm' ('IO') to make the type checker happy.
-- | The vast majority of the following struct fields are *COMPLETELY IRRELEVANT* to the
-- operation of db-sync, but I have no idea at all what happens of any of these are
-- wrong. This really needs to be a done a different way.
-- mkProtocolBcc :: GenesisConfig -> Protocol m BccBlock BccProtocol
mkProtocolInfoBcc :: GenesisConfig -> ProtocolInfo IO BccBlock
mkProtocolInfoBcc ge =
  case ge of
    GenesisBcc dnc coleGenesis sophieGenesis aurumGenesis ->
        Consensus.protocolInfoBcc
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
            , Consensus.sophieBasedInitialNonce = sophieOptimumNonce sophieGenesis
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

sophieOptimumNonce :: SophieConfig -> Nonce
sophieOptimumNonce sCfg = Nonce (Crypto.castHash . unGenesisHashSophie $ scGenesisHash sCfg)

sophieProtVer :: SyncNodeConfig -> Sophie.ProtVer
sophieProtVer dnc =
  let bver = dncColeProtocolVersion dnc in
  Sophie.ProtVer (fromIntegral $ Cole.pvMajor bver) (fromIntegral $ Cole.pvMinor bver)

