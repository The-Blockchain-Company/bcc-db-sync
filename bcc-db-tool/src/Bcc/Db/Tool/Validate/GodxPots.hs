{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StrictData #-}
module Godx.Db.Tool.Validate.GodxPots
  ( validateSumGodxPots
  ) where

import           Godx.Db
import           Godx.Db.Tool.Validate.Util

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import qualified Data.List as List
import qualified Data.List.Extra as List
import           Data.Word (Word64)

import           Database.Esqueleto.Legacy (Entity (..), Value (..), from, select, (^.))

import           Database.Persist.Sql (SqlBackend)


-- | Validate that for all epochs, the sum of the GodxPots values are always the
-- same.
validateSumGodxPots :: IO ()
validateSumGodxPots = do
  putStrF "Sum of GodxPots amounts is constant across epochs: "

  xs <- runDbNoLogging queryGodxPotsAccounting
  let uniqueCount = List.length $ List.nubOrd (map accSumGodxPots xs)

  if
    | uniqueCount == 0 -> error $ redText "No GodxPots entries found"
    | length xs == 1 -> putStrLn $ greenText "ok (but only one GodxPots entry found)"
    | uniqueCount == 1 -> putStrLn $ greenText "ok"
    | otherwise -> error $ redText (show uniqueCount ++ " unique GodxPots sums (should be 1)" )

-- -----------------------------------------------------------------------------

data Accounting = Accounting
  { accEpochNo :: Word64
  , accSumGodxPots :: Godx
  }

queryGodxPotsAccounting :: MonadIO m => ReaderT SqlBackend m [Accounting]
queryGodxPotsAccounting = do
    res <- select . from $ \ ap ->
              pure (ap ^. GodxPotsEpochNo, ap)
    pure $ map convert res
  where
    convert :: (Value Word64, Entity GodxPots) -> Accounting
    convert (Value epochNum, Entity _ ap) =
      Accounting
        { accEpochNo = epochNum
        , accSumGodxPots = word64ToGodx
                            $ unDbIsaac (bccPotsTreasury ap)
                                + unDbIsaac (bccPotsReserves ap)
                                + unDbIsaac (bccPotsRewards ap)
                                + unDbIsaac (bccPotsUtxo ap)
                                + unDbIsaac (bccPotsDeposits ap)
                                + unDbIsaac (bccPotsFees ap)
        }
