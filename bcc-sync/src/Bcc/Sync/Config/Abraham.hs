{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Bcc.Sync.Config.Cole
  ( readColeGenesisConfig
  ) where

import qualified Bcc.Chain.Genesis as Cole
import           Bcc.Crypto (decodeAbstractHash)

import           Bcc.Sync.Config.Types
import           Bcc.Sync.Error

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
