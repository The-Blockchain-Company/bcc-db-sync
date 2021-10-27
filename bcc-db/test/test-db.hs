{-# LANGUAGE OverloadedStrings #-}

import           Prelude

import           Control.Monad (when)

import           Data.Maybe (isNothing)

import           Test.Tasty (defaultMain, testGroup)

import qualified Test.IO.Godx.Db.Insert
import qualified Test.IO.Godx.Db.Migration
import qualified Test.IO.Godx.Db.Rollback
import qualified Test.IO.Godx.Db.TotalSupply

import           System.Directory (getCurrentDirectory)
import           System.Environment (lookupEnv, setEnv)
import           System.FilePath ((</>))

main :: IO ()
main = do
  -- If the env is not set, set it to default.
  mPgPassFile <- lookupEnv "PGPASSFILE"
  when (isNothing mPgPassFile) $ do
    currentDir <- getCurrentDirectory
    setEnv "PGPASSFILE" (currentDir </> "../config/pgpass-mainnet")

  defaultMain $
    testGroup "Database"
      [ Test.IO.Godx.Db.Migration.tests
      , Test.IO.Godx.Db.Insert.tests
      , Test.IO.Godx.Db.TotalSupply.tests
      , Test.IO.Godx.Db.Rollback.tests
      ]

