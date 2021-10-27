{-# LANGUAGE NoImplicitPrelude #-}
module Godx.DbSync.Era.Util
    ( liftLookupFail
    ) where

import           Godx.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import qualified Godx.Db as DB

import           Godx.Sync.Error

liftLookupFail :: Monad m => Text -> m (Either DB.LookupFail a) -> ExceptT SyncNodeError m a
liftLookupFail loc =
  firstExceptT (\lf -> NEError $ loc <> DB.renderLookupFail lf) . newExceptT

