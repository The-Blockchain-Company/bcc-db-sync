{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StrictData #-}
module Bcc.Db.Tool.Validate.BccPots
  ( validateSumBccPots
  ) where

import           Bcc.Db
import           Bcc.Db.Tool.Validate.Util

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import qualified Data.List as List
import qualified Data.List.Extra as List
import           Data.Word (Word64)

import           Database.Esqueleto.Legacy (Entity (..), Value (..), from, select, (^.))

import           Database.Persist.Sql (SqlBackend)


-- | Validate that for all epochs, the sum of the BccPots values are always the
-- same.
validateSumBccPots :: IO ()
validateSumBccPots = do
  putStrF "Sum of BccPots amounts is constant across epochs: "

  xs <- runDbNoLogging queryBccPotsAccounting
  let uniqueCount = List.length $ List.nubOrd (map accSumBccPots xs)

  if
    | uniqueCount == 0 -> error $ redText "No BccPots entries found"
    | length xs == 1 -> putStrLn $ greenText "ok (but only one BccPots entry found)"
    | uniqueCount == 1 -> putStrLn $ greenText "ok"
    | otherwise -> error $ redText (show uniqueCount ++ " unique BccPots sums (should be 1)" )

-- -----------------------------------------------------------------------------

data Accounting = Accounting
  { accEpochNo :: Word64
  , accSumBccPots :: Bcc
  }

queryBccPotsAccounting :: MonadIO m => ReaderT SqlBackend m [Accounting]
queryBccPotsAccounting = do
    res <- select . from $ \ ap ->
              pure (ap ^. BccPotsEpochNo, ap)
    pure $ map convert res
  where
    convert :: (Value Word64, Entity BccPots) -> Accounting
    convert (Value epochNum, Entity _ ap) =
      Accounting
        { accEpochNo = epochNum
        , accSumBccPots = word64ToBcc
                            $ unDbEntropic (bccPotsTreasury ap)
                                + unDbEntropic (bccPotsReserves ap)
                                + unDbEntropic (bccPotsRewards ap)
                                + unDbEntropic (bccPotsUtxo ap)
                                + unDbEntropic (bccPotsDeposits ap)
                                + unDbEntropic (bccPotsFees ap)
        }
