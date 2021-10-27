module Godx.Db.Tool.Report
  ( module X
  , Report (..)
  , runReport
  ) where

import           Godx.Db.Tool.Report.Balance (reportBalance)
import           Godx.Db.Tool.Report.StakeReward (reportLatestStakeRewards,
                   reportStakeRewardHistory)
import           Godx.Db.Tool.Report.Synced as X
import           Godx.Db.Tool.Report.Transactions (reportTransactions)

import           Data.Text (Text)


data Report
  = ReportAllRewards [Text]
  | ReportBalance [Text]
  | ReportLatestRewards [Text]
  | ReportTransactions [Text]

runReport :: Report -> IO ()
runReport report = do
  assertFullySynced
  case report of
    ReportAllRewards sas -> mapM_ reportStakeRewardHistory sas
    ReportBalance sas -> reportBalance sas
    ReportLatestRewards sas -> reportLatestStakeRewards sas
    ReportTransactions sas -> reportTransactions sas
