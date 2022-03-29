{-# LANGUAGE OverloadedStrings #-}
module Bcc.Db.Tool.Report.StakeReward.Latest
  ( reportLatestStakeRewards
  ) where

import           Bcc.Db
import           Bcc.Db.Tool.Report.Display

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import qualified Data.List as List
import           Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import           Data.Ord (Down (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time.Clock (UTCTime)
import           Data.Word (Word64)

import           Database.Esqueleto.Legacy (InnerJoin (..), Value (..), asc, desc, from, limit,
                   max_, on, orderBy, select, val, where_, (<=.), (==.), (^.))
import           Database.Persist.Sql (SqlBackend)

import           Text.Printf (printf)


reportLatestStakeRewards :: [Text] -> IO ()
reportLatestStakeRewards saddr = do
    xs <- catMaybes <$> runDbNoLogging (mapM queryLatestStakeRewards saddr)
    renderRewards xs

-- -------------------------------------------------------------------------------------------------

data EpochReward = EpochReward
  { erAddressId :: !StakeAddressId
  , erEpochNo :: !Word64
  , erDate :: !UTCTime
  , erAddress :: !Text
  , erReward :: !Bcc
  , erDelegated :: !Bcc
  , erPercent :: !Double
  }


queryLatestStakeRewards :: MonadIO m => Text -> ReaderT SqlBackend m (Maybe EpochReward)
queryLatestStakeRewards address = do
    maxEpoch <- queryMaxEpochRewardNo
    mdel <- queryDelegation maxEpoch
    maybe (pure Nothing) (fmap2 Just queryRewards) mdel
  where
    queryDelegation
        :: MonadIO m
        => Word64 -> ReaderT SqlBackend m (Maybe (StakeAddressId, Word64, UTCTime, DbEntropic))
    queryDelegation maxEpoch = do
      res <- select . from $ \ (saddr `InnerJoin` es `InnerJoin` epoch) -> do
                on (epoch ^. EpochNo ==. es ^. EpochStakeEpochNo)
                on (saddr ^. StakeAddressId ==. es ^. EpochStakeAddrId)
                where_ (saddr ^. StakeAddressView ==. val address)
                where_ (es ^. EpochStakeEpochNo <=. val maxEpoch)
                orderBy [desc (es ^. EpochStakeEpochNo)]
                limit 1
                pure (es ^. EpochStakeAddrId, es ^. EpochStakeEpochNo, epoch ^.EpochEndTime, es ^. EpochStakeAmount)
      pure $ fmap unValue4 (listToMaybe res)

    queryRewards
        :: MonadIO m
        => (StakeAddressId, Word64, UTCTime, DbEntropic)
        -> ReaderT SqlBackend m EpochReward
    queryRewards (saId, en, date, DbEntropic delegated) = do
      res <- select . from $ \ (saddr `InnerJoin` reward `InnerJoin` epoch) -> do
                on (epoch ^. EpochNo ==. reward ^. RewardEarnedEpoch)
                on (saddr ^. StakeAddressId ==. reward ^. RewardAddrId)
                where_ (epoch ^. EpochNo ==. val en)
                where_ (saddr ^. StakeAddressId ==. val saId)
                orderBy [asc (epoch ^. EpochNo)]
                pure  (reward ^. RewardAmount)
      let reward = maybe 0 (unDbEntropic . unValue) (listToMaybe res)
      pure $ EpochReward
              { erAddressId = saId
              , erEpochNo = en
              , erDate = date
              , erAddress = address
              , erReward = word64ToBcc reward
              , erDelegated = word64ToBcc delegated
              , erPercent = rewardPercent reward (if delegated == 0 then Nothing else Just delegated)
              }

    queryMaxEpochRewardNo
        :: MonadIO m
        => ReaderT SqlBackend m Word64
    queryMaxEpochRewardNo = do
      res <- select . from $ \ reward -> do
                pure (max_ (reward ^. RewardEarnedEpoch))
      pure $ fromMaybe 0 (listToMaybe $ mapMaybe unValue res)

renderRewards :: [EpochReward] -> IO ()
renderRewards xs = do
    putStrLn " epoch |                       stake_address                         |     delegated  |     reward   |  RoS (%pa)"
    putStrLn "-------+-------------------------------------------------------------+----------------+--------------+-----------"
    mapM_ renderReward (List.sortOn (Down . erDelegated) xs)
    putStrLn ""
  where
    renderReward :: EpochReward -> IO ()
    renderReward er =
      Text.putStrLn $ mconcat
        [ leftPad 6 (textShow $ erEpochNo er)
        , separator
        , erAddress er
        , separator
        , leftPad 14 (renderBcc (erDelegated er))
        , separator
        , leftPad 12 (specialRenderBcc (erReward er))
        , separator
        , Text.pack (if erPercent er == 0.0 then "   0.0" else printf "%8.3f" (erPercent er))
        ]

    specialRenderBcc :: Bcc -> Text
    specialRenderBcc bcc = if bcc == 0 then "0.0     " else renderBcc bcc

rewardPercent :: Word64 -> Maybe Word64 -> Double
rewardPercent reward mDelegated =
  case mDelegated of
    Nothing -> 0.0
    Just deleg -> 100.0 * 365.25 / 5.0 * fromIntegral reward / fromIntegral deleg

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap
