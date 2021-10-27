{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Godx.Sync.Config.Node
  ( NodeConfig (..)
  , parseNodeConfig
  ) where

-- Node really should export something like this, but doesn't and it actually seemed to
-- be easier and faster to just parse out the bits we need here.

import qualified Godx.Chain.Update as Cole

import           Godx.Crypto (RequiresNetworkMagic (..))

import           Godx.Db (textShow)

import           Godx.Sync.Config.Types

import           Godx.Prelude

import           Data.Aeson (FromJSON (..), Object, (.:), (.:?))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.Yaml as Yaml

import qualified Shardagnostic.Consensus.Godx.CanHardFork as Sophie

data NodeConfig = NodeConfig
  { ncProtocol :: !SyncProtocol
  , ncPBftSignatureThreshold :: !(Maybe Double)
  , ncColeGenesisFile :: !GenesisFile
  , ncColeGenesisHash :: !GenesisHashCole
  , ncSophieGenesisFile :: !GenesisFile
  , ncSophieGenesisHash :: !GenesisHashSophie
  , ncAurumGenesisFile :: !GenesisFile
  , ncAurumGenesisHash :: !GenesisHashAurum
  , ncRequiresNetworkMagic :: !RequiresNetworkMagic
  , ncColeSotfwareVersion :: !Cole.SoftwareVersion
  , ncColeProtocolVersion :: !Cole.ProtocolVersion

  -- Sophie hardfok parameters
  , ncSophieHardFork :: !Sophie.TriggerHardFork

  -- Allegra hardfok parameters
  , ncAllegraHardFork :: !Sophie.TriggerHardFork

  -- Jen hardfok parameters
  , ncJenHardFork :: !Sophie.TriggerHardFork

  -- Aurum hardfok parameters
  , ncAurumHardFork :: !Sophie.TriggerHardFork
  }

parseNodeConfig :: ByteString -> NodeConfig
parseNodeConfig bs =
  case Yaml.decodeEither' bs of
    Left err -> panic $ "Error parsing node config: " <> textShow err
    Right nc -> nc

-- -------------------------------------------------------------------------------------------------

instance FromJSON NodeConfig where
  parseJSON v =
      Aeson.withObject "NodeConfig" parse v
    where
      parse :: Object -> Parser NodeConfig
      parse o =
        NodeConfig
          <$> o .: "Protocol"
          <*> o .:? "PBftSignatureThreshold"
          <*> fmap GenesisFile (o .: "ColeGenesisFile")
          <*> fmap GenesisHashCole (o .: "ColeGenesisHash")
          <*> fmap GenesisFile (o .: "SophieGenesisFile")
          <*> fmap GenesisHashSophie (o .: "SophieGenesisHash")
          <*> fmap GenesisFile (o .: "AurumGenesisFile")
          <*> fmap GenesisHashAurum (o .: "AurumGenesisHash")
          <*> o .: "RequiresNetworkMagic"
          <*> parseColeSoftwareVersion o
          <*> parseColeProtocolVersion o

          <*> parseSophieHardForkEpoch o

          <*> parseAllegraHardForkEpoch o

          <*> parseJenHardForkEpoch o

          <*> parseAurumHardForkEpoch o

      parseColeProtocolVersion :: Object -> Parser Cole.ProtocolVersion
      parseColeProtocolVersion o =
        Cole.ProtocolVersion
          <$> o .: "LastKnownBlockVersion-Major"
          <*> o .: "LastKnownBlockVersion-Minor"
          <*> o .: "LastKnownBlockVersion-Alt"

      parseColeSoftwareVersion :: Object -> Parser Cole.SoftwareVersion
      parseColeSoftwareVersion o =
        Cole.SoftwareVersion
          <$> fmap Cole.ApplicationName (o .: "ApplicationName")
          <*> o .: "ApplicationVersion"

      parseSophieHardForkEpoch :: Object -> Parser Sophie.TriggerHardFork
      parseSophieHardForkEpoch o =
        asum
          [ Sophie.TriggerHardForkAtEpoch <$> o .: "TestSophieHardForkAtEpoch"
          , pure $ Sophie.TriggerHardForkAtVersion 2 -- Mainnet default
          ]

      parseAllegraHardForkEpoch :: Object -> Parser Sophie.TriggerHardFork
      parseAllegraHardForkEpoch o =
        asum
          [ Sophie.TriggerHardForkAtEpoch <$> o .: "TestAllegraHardForkAtEpoch"
          , pure $ Sophie.TriggerHardForkAtVersion 3 -- Mainnet default
          ]

      parseJenHardForkEpoch :: Object -> Parser Sophie.TriggerHardFork
      parseJenHardForkEpoch o =
        asum
          [ Sophie.TriggerHardForkAtEpoch <$> o .: "TestJenHardForkAtEpoch"
          , pure $ Sophie.TriggerHardForkAtVersion 4 -- Mainnet default
          ]

      parseAurumHardForkEpoch :: Object -> Parser Sophie.TriggerHardFork
      parseAurumHardForkEpoch o =
        asum
          [ Sophie.TriggerHardForkAtEpoch <$> o .: "TestAurumHardForkAtEpoch"
          , pure $ Sophie.TriggerHardForkAtVersion 5 -- Mainnet default
          ]
