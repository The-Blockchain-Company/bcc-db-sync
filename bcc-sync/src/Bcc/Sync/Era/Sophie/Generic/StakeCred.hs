{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Bcc.Sync.Era.Sophie.Generic.StakeCred
  ( StakeCred (..)
  , toStakeCred
  ) where

import           Bcc.Prelude hiding (Show)

import qualified Bcc.Ledger.Address as Ledger
import qualified Bcc.Ledger.BaseTypes as Ledger
import qualified Bcc.Ledger.Credential as Ledger
import qualified Bcc.Ledger.Keys as Ledger

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
