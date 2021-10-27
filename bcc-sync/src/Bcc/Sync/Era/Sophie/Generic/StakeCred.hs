{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Godx.Sync.Era.Sophie.Generic.StakeCred
  ( StakeCred (..)
  , toStakeCred
  ) where

import           Godx.Prelude hiding (Show)

import qualified Godx.Ledger.Address as Ledger
import qualified Godx.Ledger.BaseTypes as Ledger
import qualified Godx.Ledger.Credential as Ledger
import qualified Godx.Ledger.Keys as Ledger

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS

import           Prelude (Show (..))

newtype StakeCred
  = StakeCred { unStakeCred :: ByteString }
  deriving (Eq, Ord)

instance Show StakeCred where
  show (StakeCred bs) = BS.unpack $ Base16.encode bs

toStakeCred :: Ledger.Network -> Ledger.Credential 'Ledger.Staking era -> StakeCred
toStakeCred network cred =
  StakeCred $ Ledger.serialiseRewardAcnt (Ledger.RewardAcnt network cred)
