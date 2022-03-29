{-# LANGUAGE NoImplicitPrelude #-}
module Bcc.DbSync.Era.Util
    ( liftLookupFail
    ) where

import           Bcc.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import qualified Bcc.Db as DB

import           Bcc.Sync.Error

liftLookupFail :: Monad m => Text -> m (Either DB.LookupFail a) -> ExceptT SyncNodeError m a
liftLookupFail loc =
  firstExceptT (\lf -> NEError $ loc <> DB.renderLookupFail lf) . newExceptT

