{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Godx.Db.Types
  ( Godx (..)
  , DbIsaac (..)
  , DbInt65 (..)
  , DbWord64 (..)
  , RewardSource (..)
  , SyncState (..)
  , ScriptPurpose (..)
  , ScriptType (..)
  , deltaCoinToDbInt65
  , integerToDbInt65
  , isaacToGodx
  , renderGodx
  , scientificToGodx
  , readDbInt65
  , showDbInt65
  , readRewardSource
  , readScriptPurpose
  , readScriptType
  , readSyncState
  , renderScriptPurpose
  , renderScriptType
  , renderSyncState
  , rewardTypeToSource
  , showRewardSource
  , word64ToGodx
  ) where

import           Godx.Ledger.Coin (DeltaCoin (..))

import           Data.Aeson.Encoding (unsafeToEncoding)
import           Data.Aeson.Types (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Builder as BS (string8)
import           Data.Fixed (Micro, showFixed)
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word64)

import           GHC.Generics (Generic)

import           Quiet (Quiet (..))

import qualified Sophie.Spec.Ledger.Rewards as Sophie


newtype Godx = Godx
  { unGodx :: Micro
  } deriving (Eq, Num, Ord, Generic)

instance FromJSON Godx where
  parseJSON =
    Aeson.withScientific "Godx" (pure . scientificToGodx)

instance ToJSON Godx where
    --toJSON (Godx bcc) = Data.Aeson.Types.Number $ fromRational $ toRational bcc
    -- `Number` results in it becoming `7.3112484749601107e10` while the old explorer is returning `73112484749.601107`
    toEncoding (Godx bcc) =
        unsafeToEncoding $   -- convert ByteString to Aeson's Encoding
        BS.string8 $         -- convert String to ByteString using Latin1 encoding
        showFixed True bcc   -- convert Micro to String chopping off trailing zeros

    toJSON = error "Godx.toJSON not supported due to numeric issues. Use toEncoding instead."

instance Show Godx where
    show (Godx bcc) = showFixed True bcc

-- This is horrible. Need a 'Word64' with an extra sign bit.
data DbInt65
  = PosInt65 !Word64
  | NegInt65 !Word64
  deriving (Eq, Generic, Show)

-- Newtype wrapper around Word64 so we can hand define a PersistentField instance.
newtype DbIsaac
  = DbIsaac { unDbIsaac :: Word64 }
  deriving (Eq, Generic)
  deriving (Read, Show) via (Quiet DbIsaac)

-- Newtype wrapper around Word64 so we can hand define a PersistentField instance.
newtype DbWord64
  = DbWord64 { unDbWord64 :: Word64 }
  deriving (Eq, Generic)
  deriving (Read, Show) via (Quiet DbWord64)

data RewardSource
  = RwdMember
  | RwdLeader
  | RwdReserves
  | RwdTreasury
  deriving (Bounded, Enum, Eq, Ord, Show)

data SyncState
  = SyncLagging         -- Local tip is lagging the global chain tip.
  | SyncFollowing       -- Local tip is following global chain tip.
  deriving (Eq, Show)

data ScriptPurpose
  = Spend
  | Mint
  | Cert
  | Rewrd
  deriving (Eq, Generic, Show)

data ScriptType
  = Timelock
  | Zerepoch
  deriving (Eq, Generic, Show)

deltaCoinToDbInt65 :: DeltaCoin -> DbInt65
deltaCoinToDbInt65 (DeltaCoin dc) =
  if dc < 0
    then NegInt65 (fromIntegral $ abs dc)
    else PosInt65 (fromIntegral dc)

integerToDbInt65 :: Integer -> DbInt65
integerToDbInt65 i =
  if i >= 0
    then PosInt65 (fromIntegral i)
    else NegInt65 (fromIntegral $ negate i)

isaacToGodx :: Micro -> Godx
isaacToGodx ll =
  Godx (ll / 1000000)

renderGodx :: Godx -> Text
renderGodx (Godx a) = Text.pack (show a)

scientificToGodx :: Scientific -> Godx
scientificToGodx s =
  word64ToGodx $ floor (s * 1000000)

readDbInt65 :: String -> DbInt65
readDbInt65 str =
  case str of
    ('-':rest) -> NegInt65 $ read rest
    _other -> PosInt65 $ read str

showDbInt65 :: DbInt65 -> String
showDbInt65 i65 =
  case i65 of
    PosInt65 w -> show w
    NegInt65 0 -> "0"
    NegInt65 w -> '-' : show w

readRewardSource :: Text -> RewardSource
readRewardSource str =
  case str of
    "member" -> RwdMember
    "leader" -> RwdLeader
    "reserves" -> RwdReserves
    "treasury" -> RwdTreasury
    -- This should never happen. On the Postgres side we defined an ENUM with
    -- only the two values as above.
    _other -> error $ "readRewardSource: Unknown RewardSource " ++ Text.unpack str

readSyncState :: String -> SyncState
readSyncState str =
  case str of
    "lagging" -> SyncLagging
    "following" -> SyncFollowing
    -- This should never happen. On the Postgres side we defined an ENUM with
    -- only the two values as above.
    _other -> error $ "readSyncState: Unknown SyncState " ++ str

renderSyncState :: SyncState -> Text
renderSyncState ss =
  case ss of
    SyncFollowing -> "following"
    SyncLagging -> "lagging"

rewardTypeToSource :: Sophie.RewardType -> RewardSource
rewardTypeToSource rt =
  case rt of
    Sophie.LeaderReward -> RwdLeader
    Sophie.MemberReward -> RwdMember

renderScriptPurpose :: ScriptPurpose -> Text
renderScriptPurpose ss =
  case ss of
    Spend -> "spend"
    Mint -> "mint"
    Cert -> "cert"
    Rewrd -> "reward"

readScriptPurpose :: String -> ScriptPurpose
readScriptPurpose str =
  case str of
    "spend" -> Spend
    "mint" -> Mint
    "cert" -> Cert
    "reward" -> Rewrd
    _other -> error $ "readScriptPurpose: Unknown ScriptPurpose " ++ str

showRewardSource :: RewardSource -> Text
showRewardSource rs =
  case rs of
    RwdMember -> "member"
    RwdLeader -> "leader"
    RwdReserves -> "reserves"
    RwdTreasury -> "treasury"

renderScriptType :: ScriptType -> Text
renderScriptType st =
  case st of
    Timelock -> "timelock"
    Zerepoch -> "zerepoch"

readScriptType :: String -> ScriptType
readScriptType str =
  case str of
    "timelock" -> Timelock
    "zerepoch" -> Zerepoch
    _other -> error $ "readScriptType: Unknown ScriptType " ++ str

word64ToGodx :: Word64 -> Godx
word64ToGodx w =
  Godx (fromIntegral w / 1000000)
