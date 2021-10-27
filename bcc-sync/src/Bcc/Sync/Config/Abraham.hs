{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Godx.Sync.Config.Cole
  ( readColeGenesisConfig
  ) where

import qualified Godx.Chain.Genesis as Cole
import           Godx.Crypto (decodeAbstractHash)

import           Godx.Sync.Config.Types
import           Godx.Sync.Error

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither)

readColeGenesisConfig
        :: SyncNodeConfig
        -> ExceptT SyncNodeError IO Cole.Config
readColeGenesisConfig enc = do
  let file = unGenesisFile $ dncColeGenesisFile enc
  genHash <- firstExceptT NEError
                . hoistEither
                $ decodeAbstractHash (unGenesisHashCole $ dncColeGenesisHash enc)
  firstExceptT (NEColeConfig file)
                $ Cole.mkConfigFromFile (dncRequiresNetworkMagic enc) file genHash
