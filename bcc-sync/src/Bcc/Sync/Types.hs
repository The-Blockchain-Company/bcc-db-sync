{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Bcc.Sync.Types
  ( BlockDetails (..)
  , BlockEra (..)
  , BccBlock
  , BccPoint
  , BccProtocol
  , EpochSlot (..)
  , FetchResult (..)
  , PoolKeyHash
  , SlotDetails (..)
  , Block (..)
  , MetricSetters (..)
  , PoolFetchRetry (..)
  , Retry (..)
  ) where

import           Bcc.Prelude hiding (Meta)

import qualified Bcc.Ledger.Keys as Ledger

import           Bcc.Db (PoolHashId, PoolMetaHash, PoolMetadataRefId, PoolOfflineData,
                   PoolOfflineFetchError, PoolUrl)

import           Bcc.Sync.Config.Types (BccBlock, BccProtocol)

import           Bcc.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))

import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (POSIXTime)

import           Shardagnostic.Consensus.Bcc.Block (StandardCrypto)
import           Shardagnostic.Network.Block (BlockNo, Point)


type BccPoint = Point BccBlock

type PoolKeyHash = Ledger.KeyHash 'Ledger.StakePool StandardCrypto

data BlockDetails = BlockDetails
  { bdBlock :: !BccBlock
  , bdSlot :: !SlotDetails
  }

data BlockEra
  = Sophie
  | Allegra
  | Jen
  | Aurum
  deriving (Eq, Show)
  
-- | Slot within an Epoch.
newtype EpochSlot = EpochSlot
  { unEpochSlot :: Word64
  } deriving (Eq, Ord, Show)

data FetchResult
    = ResultMetadata !PoolOfflineData
    | ResultError !PoolOfflineFetchError
    deriving Show

data SlotDetails = SlotDetails
  { sdSlotTime :: !UTCTime
  , sdCurrentTime :: !UTCTime
  , sdEpochNo :: !EpochNo
  , sdEpochSlot :: !EpochSlot
  , sdEpochSize :: !EpochSize
  } deriving (Eq, Show)

-- The hash must be unique!
data Block = Block
  { bHash :: !ByteString
  , bEpochNo :: !EpochNo
  , bSlotNo  :: !SlotNo
  , bBlockNo :: !BlockNo
  } deriving (Eq, Show)

-- The metrics we use.
-- Kept as a separate struct and do not put into environment because
-- when we need to test functions using this we need to initialize the
-- whole environment and not just pass in the layer. This shows clearly
-- that it needs to remain a separate parameter passed around where needed.
data MetricSetters = MetricSetters
  { metricsSetNodeBlockHeight :: BlockNo -> IO ()
  , metricsSetDbQueueLength :: Natural -> IO ()
  , metricsSetDbBlockHeight :: BlockNo -> IO ()
  , metricsSetDbSlotHeight :: SlotNo -> IO ()
  }

data PoolFetchRetry = PoolFetchRetry
  { pfrPoolHashId :: !PoolHashId
  , pfrReferenceId :: !PoolMetadataRefId
  , pfrPoolUrl :: !PoolUrl
  , pfrPoolMDHash :: !PoolMetaHash
  , pfrRetry :: !Retry
  } deriving (Show)


data Retry = Retry
  { retryFetchTime :: !POSIXTime -- Time last time time
  , retryRetryTime :: !POSIXTime -- Time to retry
  , retryCount :: !Word
  } deriving (Eq, Show, Generic)
