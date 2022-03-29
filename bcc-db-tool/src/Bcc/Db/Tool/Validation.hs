module Bcc.Db.Tool.Validation
  ( LedgerValidationParams (..)
  , runDbValidation
  , runLedgerValidation
  ) where

import           Bcc.Db.Tool.Validate.BccPots (validateSumBccPots)
import           Bcc.Db.Tool.Validate.BlockProperties (validateBlockProperties)
import           Bcc.Db.Tool.Validate.BlockTxs (validateEpochBlockTxs)
import           Bcc.Db.Tool.Validate.EpochTable (validateEpochTable)
import           Bcc.Db.Tool.Validate.Ledger (LedgerValidationParams (..), validateLedger)
import           Bcc.Db.Tool.Validate.PoolOwner (validateAllPoolsHaveOwners)
import           Bcc.Db.Tool.Validate.TotalSupply (validateTotalSupplyDecreasing)
import           Bcc.Db.Tool.Validate.TxAccounting (validateTxAccounting)

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
  validateSumBccPots

slowValidations :: IO ()
slowValidations = do
  validateEpochTable
  validateEpochBlockTxs
  validateTotalSupplyDecreasing
