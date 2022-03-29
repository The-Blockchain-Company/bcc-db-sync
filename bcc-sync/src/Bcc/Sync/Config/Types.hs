{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bcc.Sync.Config.Types
  ( AllegraToJen
  , ColeToSophie
  , BccBlock
  , BccProtocol
  , ConfigFile (..)
  , SyncCommand (..)
  , SyncNodeParams (..)
  , SyncProtocol (..)
  , GenesisFile (..)
  , GenesisHashSophie (..)
  , GenesisHashCole (..)
  , GenesisHashAurum (..)
  , SyncNodeConfig (..)
  , SyncPreConfig (..)
  , LedgerStateDir (..)
  , MigrationDir (..)
  , JenToAurum
  , LogFileDir (..)
  , NetworkName (..)
  , NodeConfigFile (..)
  , SophieToAllegra
  , SocketPath (..)
  , adjustGenesisFilePath
  , adjustNodeConfigFilePath
  , pcNodeConfigFilePath
  ) where

import           Bcc.Prelude

import qualified Bcc.BM.Configuration as Logging
import qualified Bcc.BM.Data.Configuration as Logging

import qualified Bcc.Chain.Update as Cole

import           Bcc.Crypto (RequiresNetworkMagic (..))
import qualified Bcc.Crypto.Hash as Crypto

import           Bcc.Ledger.Allegra (AllegraEra)

import           Bcc.Slotting.Slot (SlotNo (..))

import           Data.Aeson (FromJSON (..), Object, Value (..), (.:), (.:?))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (Parser, typeMismatch)

import           Shardagnostic.Consensus.Cole.Ledger (ColeBlock (..))
import           Shardagnostic.Consensus.Bcc.Block (AurumEra, JenEra, SophieEra)
import qualified Shardagnostic.Consensus.Bcc.Block as Bcc
import qualified Shardagnostic.Consensus.Bcc.CanHardFork as Sophie
import           Shardagnostic.Consensus.Bcc.Node (ProtocolTransitionParamsSophieBased)
import qualified Shardagnostic.Consensus.HardFork.Combinator.Basics as Bcc
import           Shardagnostic.Consensus.Sophie.Eras (StandardSophie)
import qualified Shardagnostic.Consensus.Sophie.Ledger.Block as Sophie
import           Shardagnostic.Consensus.Sophie.Protocol (StandardCrypto)

newtype MigrationDir = MigrationDir
  { unMigrationDir :: FilePath
  }

newtype LogFileDir = LogFileDir
  { unLogFileDir :: FilePath
  }

type BccBlock =
        Bcc.HardForkBlock
            (Bcc.BccEras StandardCrypto)

type BccProtocol =
        Bcc.HardForkProtocol
            '[ ColeBlock
            , Sophie.SophieBlock StandardSophie
            , Sophie.SophieBlock Bcc.StandardAllegra
            , Sophie.SophieBlock Bcc.StandardJen
            ]

type ColeToSophie =
        ProtocolTransitionParamsSophieBased (SophieEra StandardCrypto)

type SophieToAllegra =
        ProtocolTransitionParamsSophieBased (AllegraEra StandardCrypto)

type AllegraToJen =
        ProtocolTransitionParamsSophieBased (JenEra StandardCrypto)

type JenToAurum =
        ProtocolTransitionParamsSophieBased (AurumEra StandardCrypto)

newtype ConfigFile = ConfigFile
  { unConfigFile :: FilePath
  }

data SyncCommand
  = CmdRun !SyncNodeParams
  | CmdVersion

-- | The product type of all command line arguments
data SyncNodeParams = SyncNodeParams
  { enpConfigFile :: !ConfigFile
  , enpSocketPath :: !SocketPath
  , enpLedgerStateDir :: !LedgerStateDir
  , enpMigrationDir :: !MigrationDir
  , enpMaybeRollback :: !(Maybe SlotNo)
  }

-- May have other constructors when we are preparing for a HFC event.
data SyncProtocol
  = SyncProtocolBcc
  deriving Show

data SyncNodeConfig = SyncNodeConfig
  { dncNetworkName :: !NetworkName
  , dncLoggingConfig :: !Logging.Configuration
  , dncNodeConfigFile :: !NodeConfigFile
  , dncProtocol :: !SyncProtocol
  , dncRequiresNetworkMagic :: !RequiresNetworkMagic
  , dncEnableLogging :: !Bool
  , dncEnableMetrics :: !Bool
  , dncPrometheusPort :: !Int
  , dncPBftSignatureThreshold :: !(Maybe Double)
  , dncColeGenesisFile :: !GenesisFile
  , dncColeGenesisHash :: !GenesisHashCole
  , dncSophieGenesisFile :: !GenesisFile
  , dncSophieGenesisHash :: !GenesisHashSophie
  , dncAurumGenesisFile :: !GenesisFile
  , dncAurumGenesisHash :: !GenesisHashAurum
  , dncColeSoftwareVersion :: !Cole.SoftwareVersion
  , dncColeProtocolVersion :: !Cole.ProtocolVersion

  , dncSophieHardFork :: !Sophie.TriggerHardFork
  , dncAllegraHardFork :: !Sophie.TriggerHardFork
  , dncJenHardFork :: !Sophie.TriggerHardFork
  , dncAurumHardFork :: !Sophie.TriggerHardFork
  }

data SyncPreConfig = SyncPreConfig
  { pcNetworkName :: !NetworkName
  , pcLoggingConfig :: !Logging.Representation
  , pcNodeConfigFile :: !NodeConfigFile
  , pcEnableLogging :: !Bool
  , pcEnableMetrics :: !Bool
  , pcPrometheusPort :: !Int
  }

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  } deriving Show

newtype GenesisHashCole = GenesisHashCole
  { unGenesisHashCole :: Text
  } deriving newtype (Eq, Show)

newtype GenesisHashSophie = GenesisHashSophie
  { unGenesisHashSophie :: Crypto.Hash Crypto.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype GenesisHashAurum = GenesisHashAurum
  { unGenesisHashAurum :: Crypto.Hash Crypto.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)


newtype LedgerStateDir = LedgerStateDir
  {  unLedgerStateDir :: FilePath
  } deriving Show

newtype NetworkName = NetworkName
  { unNetworkName :: Text
  } deriving Show

newtype NodeConfigFile = NodeConfigFile
  { unNodeConfigFile :: FilePath
  } deriving Show

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  } deriving Show

adjustGenesisFilePath :: (FilePath -> FilePath) -> GenesisFile -> GenesisFile
adjustGenesisFilePath f (GenesisFile p) = GenesisFile (f p)

adjustNodeConfigFilePath :: (FilePath -> FilePath) -> NodeConfigFile -> NodeConfigFile
adjustNodeConfigFilePath f (NodeConfigFile p) = NodeConfigFile (f p)

pcNodeConfigFilePath :: SyncPreConfig -> FilePath
pcNodeConfigFilePath = unNodeConfigFile . pcNodeConfigFile

-- -------------------------------------------------------------------------------------------------

instance FromJSON SyncPreConfig where
  parseJSON o =
    Aeson.withObject "top-level" parseGenSyncNodeConfig o

parseGenSyncNodeConfig :: Object -> Parser SyncPreConfig
parseGenSyncNodeConfig o =
  SyncPreConfig
    <$> fmap NetworkName (o .: "NetworkName")
    <*> parseJSON (Object o)
    <*> fmap NodeConfigFile (o .: "NodeConfigFile")
    <*> o .: "EnableLogging"
    <*> o .: "EnableLogMetrics"
    <*> fmap (fromMaybe 8080) (o .:? "PrometheusPort")

instance FromJSON SyncProtocol where
  parseJSON o =
    case o of
      String "Bcc" -> pure SyncProtocolBcc
      x -> typeMismatch "Protocol" x
