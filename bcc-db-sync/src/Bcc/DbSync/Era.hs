{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Bcc.DbSync.Era
  ( module X
  , insertValidateGenesisDist
  ) where

import           Bcc.Prelude

import           Bcc.BM.Data.Trace (Trace)

import           Bcc.Sync.Config
import           Bcc.Sync.Error

import qualified Bcc.DbSync.Era.Cole.Genesis as Cole
import qualified Bcc.DbSync.Era.Sophie.Genesis as Sophie
import           Bcc.DbSync.Era.Sophie.Offline as X

import           Database.Persist.Sql (SqlBackend)

insertValidateGenesisDist
    :: SqlBackend -> Trace IO Text -> NetworkName -> GenesisConfig
    -> ExceptT SyncNodeError IO ()
insertValidateGenesisDist backend trce nname genCfg =
  case genCfg of
    GenesisBcc _ bCfg sCfg _aCfg -> do
      Cole.insertValidateGenesisDist backend trce (unNetworkName nname) bCfg
      Sophie.insertValidateGenesisDist backend trce (unNetworkName nname) (scConfig sCfg)
