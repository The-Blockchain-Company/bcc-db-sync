{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Bcc.DbSync.Era.Sophie.Validate
  ( validateEpochRewardsBefore
  ) where

import           Bcc.Prelude hiding (from, on)

import           Bcc.BM.Trace (Trace, logError)

import qualified Bcc.Db as Db

import           Bcc.Sync.Util

import           Bcc.Slotting.Slot (EpochNo (..))

import           Control.Monad.Trans.Control (MonadBaseControl)

import           Database.Esqueleto.Legacy (Value (..), from, select, sum_, val, where_, (==.),
                   (^.))

import           Database.Persist.Sql (SqlBackend)


validateEpochRewardsBefore
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> EpochNo
    -> ReaderT SqlBackend m ()
validateEpochRewardsBefore tracer epochNo = do
  actual <- queryEpochRewardTotal epochNo
  unless (actual == 0) $ do
    mExpected <- queryEpochRewardTotalReceived epochNo
    case mExpected of
      Nothing ->
        liftIO . logError tracer $ mconcat
                    [ "validateEpochRewardsBefore: no expected total for rewards earned in epoch "
                    , textShow (unEpochNo epochNo)
                    ]
      Just expected ->
        when (actual /= expected) .
          liftIO .
            logError tracer $ mconcat
                [ "validateEpochRewardsBefore: rewards earned in epoch "
                , textShow (unEpochNo epochNo), " expected total of ", textShow expected
                , " BCC but got " , textShow actual, " BCC"
                ]

-- -------------------------------------------------------------------------------------------------

queryEpochRewardTotal
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> ReaderT SqlBackend m Db.Bcc
queryEpochRewardTotal (EpochNo epochNo) = do
  res <- select . from $ \ rwd -> do
            where_ (rwd ^. Db.RewardEarnedEpoch ==. val epochNo)
            pure (sum_ $ rwd ^. Db.RewardAmount)
  pure $ Db.unValueSumBcc (listToMaybe res)

queryEpochRewardTotalReceived
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> ReaderT SqlBackend m (Maybe Db.Bcc)
queryEpochRewardTotalReceived (EpochNo epochNo) = do
  res <- select . from $ \ ertr -> do
            where_ (ertr ^. Db.EpochRewardTotalReceivedEarnedEpoch==. val epochNo)
            pure (ertr ^. Db.EpochRewardTotalReceivedAmount)
  pure $ Db.word64ToBcc . Db.unDbEntropic . unValue <$> listToMaybe res
