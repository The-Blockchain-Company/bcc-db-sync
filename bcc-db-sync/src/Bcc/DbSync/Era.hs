{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Godx.DbSync.Era
  ( module X
  , insertValidateGenesisDist
  ) where

import           Godx.Prelude

import           Godx.BM.Data.Trace (Trace)

import           Godx.Sync.Config
import           Godx.Sync.Error

import qualified Godx.DbSync.Era.Cole.Genesis as Cole
import qualified Godx.DbSync.Era.Sophie.Genesis as Sophie
import           Godx.DbSync.Era.Sophie.Offline as X

import           Database.Persist.Sql (SqlBackend)

insertValidateGenesisDist
    :: SqlBackend -> Trace IO Text -> NetworkName -> GenesisConfig
    -> ExceptT SyncNodeError IO ()
insertValidateGenesisDist backend trce nname genCfg =
  case genCfg of
    GenesisGodx _ bCfg sCfg _aCfg -> do
      Cole.insertValidateGenesisDist backend trce (unNetworkName nname) bCfg
      Sophie.insertValidateGenesisDist backend trce (unNetworkName nname) (scConfig sCfg)
