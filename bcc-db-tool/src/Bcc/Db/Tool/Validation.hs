module Godx.Db.Tool.Validation
  ( LedgerValidationParams (..)
  , runDbValidation
  , runLedgerValidation
  ) where

import           Godx.Db.Tool.Validate.GodxPots (validateSumGodxPots)
import           Godx.Db.Tool.Validate.BlockProperties (validateBlockProperties)
import           Godx.Db.Tool.Validate.BlockTxs (validateEpochBlockTxs)
import           Godx.Db.Tool.Validate.EpochTable (validateEpochTable)
import           Godx.Db.Tool.Validate.Ledger (LedgerValidationParams (..), validateLedger)
import           Godx.Db.Tool.Validate.PoolOwner (validateAllPoolsHaveOwners)
import           Godx.Db.Tool.Validate.TotalSupply (validateTotalSupplyDecreasing)
import           Godx.Db.Tool.Validate.TxAccounting (validateTxAccounting)

runDbValidation :: IO ()
runDbValidation = do
  fastValidations
  slowValidations

runLedgerValidation :: LedgerValidationParams -> IO ()
runLedgerValidation =
  validateLedger

-- -------------------------------------------------------------------------------------------------

fastValidations :: IO ()
fastValidations = do
  validateAllPoolsHaveOwners
  validateTxAccounting
  validateBlockProperties
  validateSumGodxPots

slowValidations :: IO ()
slowValidations = do
  validateEpochTable
  validateEpochBlockTxs
  validateTotalSupplyDecreasing
