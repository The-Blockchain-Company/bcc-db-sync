{-# LANGUAGE StrictData #-}
module Godx.Db.Tool.Validate.TotalSupply
  ( validateTotalSupplyDecreasing
  ) where

import           Godx.Db.Tool.Validate.Util

import           Data.Word (Word64)

import           Godx.Db

import           System.Random (randomRIO)


-- | Validate that the total supply is decreasing.
-- This is only true for the Cole error where transaction fees are burnt.
validateTotalSupplyDecreasing :: IO ()
validateTotalSupplyDecreasing = do
    test <- genTestParameters

    putStrF $ "Total supply + fees + deposit - withdrawals at block " ++ show (testBlockNo test)
            ++ " is same as genesis supply: "

    accounting <- queryInitialSupply (testBlockNo test)

    let total = accSupply accounting + accFees accounting + accDeposit accounting - accWithdrawals accounting

    if genesisSupply test == total
      then putStrLn $ greenText "ok"
      else error $ redText (show (genesisSupply test) ++ " /= " ++ show total)

-- -----------------------------------------------------------------------------

data Accounting = Accounting
  { accFees :: Godx
  , accDeposit :: Godx
  , accWithdrawals :: Godx
  , accSupply :: Godx
  }

data TestParams = TestParams
  { testBlockNo :: Word64
  , genesisSupply :: Godx
  }

genTestParameters :: IO TestParams
genTestParameters = do
  mlatest <- runDbNoLogging queryLatestBlockNo
  case mlatest of
    Nothing -> error "Godx.Db.Tool.Validation: Empty database"
    Just latest ->
      TestParams
          <$> randomRIO (1, latest - 1)
          <*> runDbNoLogging queryGenesisSupply


queryInitialSupply :: Word64 -> IO Accounting
queryInitialSupply blkNo =
  -- Run all queries in a single transaction.
  runDbNoLogging $
    Accounting
      <$> queryFeesUpToBlockNo blkNo
      <*> queryDepositUpToBlockNo blkNo
      <*> queryWithdrawalsUpToBlockNo blkNo
      <*> fmap2 utxoSetSum queryUtxoAtBlockNo blkNo
