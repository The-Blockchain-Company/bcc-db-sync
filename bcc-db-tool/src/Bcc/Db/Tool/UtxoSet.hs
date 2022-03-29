{-# LANGUAGE OverloadedStrings #-}
module Bcc.Db.Tool.UtxoSet
  ( utxoSetAtSlot
  ) where

import           Bcc.Chain.Common (decodeAddressBase58, isRedeemAddress)

import           Data.Time.Clock (UTCTime)
import           Data.Word (Word64)

import           Bcc.Db

import           Data.ByteString.Char8 (ByteString)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           System.Exit (exitSuccess)
import           System.IO (IOMode (..), withFile)

utxoSetAtSlot :: Word64 -> IO ()
utxoSetAtSlot slotNo = do
  (genesisSupply, utxoSet, fees, eUtcTime) <- queryAtSlot slotNo

  let supply = utxoSetSum utxoSet
  let aggregated = aggregateUtxos utxoSet
  let (accept, reject) = partitionUtxos aggregated

  if null aggregated
    then reportSlotDate slotNo eUtcTime
    else do
      putStr $ unlines
        [ "\nGenesis supply: " ++ show genesisSupply ++ " Bcc"
        , "\nAt slot number " ++ show slotNo ++ ":"
        , "   Date: " ++ either (const "unknown") show eUtcTime
        , "   Supply: " ++ show supply ++ " Bcc"
        , "   Fees: " ++ show fees ++ " Bcc"
        , "   Supply + fees == genesis supply: " ++ show (fees + supply == genesisSupply)
        , "\nFrom database:"
        , "  Utxo entries: " ++ show (length utxoSet)
        , "  Utxo supply : " ++ show (utxoSetSum utxoSet) ++ " Bcc"
        , "\nAfter aggregation:"
        , "  Utxo entries: " ++ show (length aggregated)
        , "  Utxo supply : " ++ show (sum $ map snd aggregated) ++ " Entropic"
        , "\nAfter paritioning:"
        , "  Accepted Utxo entries: " ++ show (length accept)
        , "  Rejected Utxo entries: " ++ show (length reject)
        , "  Accepted Utxo supply: " ++ show (sum $ map snd accept) ++ " Entropic"
        , "  Rejected Utxo supply: " ++ show (sum $ map snd reject) ++ " Entropic"
        , "  Accepted + rejected == totalSupply: "
              ++ show (sum (map snd accept) + sum (map snd reject) == sum (map snd aggregated))
        , ""
        ]

      writeUtxos ("utxo-accept-" ++ show slotNo ++ ".json") accept
      writeUtxos ("utxo-reject-" ++ show slotNo ++ ".json") reject
      putStrLn ""

-- -----------------------------------------------------------------------------

aggregateUtxos :: [(TxOut, a)] -> [(Text, Word64)]
aggregateUtxos xs =
  List.sortOn (Text.length . fst)
    . Map.toList
    . Map.fromListWith (+)
    $ map (\(x, _) -> (txOutAddress x, unDbEntropic (txOutValue x))) xs

isRedeemTextAddress :: Text -> Bool
isRedeemTextAddress addr =
  -- Only Cole has redeem addresses and only Cole uses Base58 encoding.
  case decodeAddressBase58 addr of
    Right a -> isRedeemAddress a
    Left _ -> False

partitionUtxos :: [(Text, Word64)] -> ([(Text, Word64)], [(Text, Word64)])
partitionUtxos =
    List.partition accept
  where
    -- Accept addresses that for which this predicate is True.
    accept :: (Text, a) -> Bool
    accept (addr, _) =
      Text.length addr <= 180 && not (isRedeemTextAddress addr)

queryAtSlot :: Word64 -> IO (Bcc, [(TxOut, ByteString)], Bcc, Either LookupFail UTCTime)
queryAtSlot slotNo =
  -- Run the following queries in a single transaction.
  runDbNoLogging $ do
    (,,,) <$> queryGenesisSupply
            <*> queryUtxoAtSlotNo slotNo
            <*> queryFeesUpToSlotNo slotNo
             <*> querySlotUtcTime slotNo

reportSlotDate :: Word64 -> Either a UTCTime -> IO ()
reportSlotDate slotNo eUtcTime = do
  case eUtcTime of
    Left _ -> putStrLn "\nDatabase not initialized or not accessible"
    Right time -> putStrLn $ "\nSlot number " ++ show slotNo ++ " will occur at " ++ show time ++ ".\n"
  exitSuccess

showUtxo :: (Text, Word64) -> Text
showUtxo (addr, value) =
  mconcat
    [ "    {\n"
    , "      \"address\": \"",  addr, "\",\n"
    , "      \"value\": ", textShow value, "\n"
    , "    }"
    ]

utxoSetSum :: [(TxOut, a)] -> Bcc
utxoSetSum xs =
  word64ToBcc . sum $ map (unDbEntropic . txOutValue . fst) xs

writeUtxos :: FilePath -> [(Text, Word64)] -> IO ()
writeUtxos fname xs = do
  putStrLn $ "Writing file: " ++ fname
  withFile fname WriteMode $ \ handle -> do
    Text.hPutStrLn handle "{\n  \"fund\": ["
    mapM_ (Text.hPutStr handle) (List.intersperse ",\n" $ map showUtxo xs)
    Text.hPutStrLn handle "\n  ]\n}"
