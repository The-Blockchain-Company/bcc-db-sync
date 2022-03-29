{-# LANGUAGE ScopedTypeVariables #-}

module Test.IO.Bcc.Db.TotalSupply
  ( tests
  ) where

import           Bcc.Db

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)

import           Test.IO.Bcc.Db.Util

tests :: TestTree
tests =
  testGroup "TotalSupply"
    [ testCase "Initial supply correct" initialSupplyTest
    ]


initialSupplyTest :: IO ()
initialSupplyTest =
  runDbNoLogging $ do
    -- Delete the blocks if they exist.
    deleteAllBlocksCascade

    -- Set up initial supply.
    slid <- insertSlotLeader testSlotLeader
    bid0 <- insertBlock (mkBlock 0 slid)
    (tx0Ids :: [TxId]) <- mapM insertTx $ mkTxs bid0 4
    mapM_ (insertTxOut . mkTxOut bid0) tx0Ids
    count <- queryBlockCount
    assertBool ("Block count should be 1, got " ++ show count) (count == 1)
    supply0 <- queryTotalSupply
    assertBool "Total supply should not be > 0" (supply0 > Bcc 0)

    -- Spend from the Utxo set.
    bid1 <- insertBlock (mkBlock 1 slid)
    tx1Id <- insertTx $
                Tx
                  { txHash = mkTxHash bid1 1
                  , txBlockId = bid1
                  , txBlockIndex = 0
                  , txOutSum = DbEntropic 500000000
                  , txFee = DbEntropic 100
                  , txDeposit = 0
                  , txSize = 123
                  , txInvalidHereafter = Nothing
                  , txInvalidBefore = Nothing
                  , txValidContract = True
                  , txScriptSize = 0
                  }
    _ <- insertTxIn (TxIn tx1Id (head tx0Ids) 0 Nothing)
    let addr = mkAddressHash bid1 tx1Id
    _ <- insertTxOut $ TxOut tx1Id 0 (Text.pack addr) (BS.pack addr) False Nothing Nothing (DbEntropic 500000000) Nothing
    supply1 <- queryTotalSupply
    assertBool ("Total supply should be < " ++ show supply0) (supply1 < supply0)
