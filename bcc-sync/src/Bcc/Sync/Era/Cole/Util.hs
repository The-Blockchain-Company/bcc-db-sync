{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Bcc.Sync.Era.Cole.Util
  ( blockHash
  , blockNumber
  , blockPayload
  , blockPreviousHash
  , boundaryEpochNumber
  , configSlotDuration
  , epochNumber
  , genesisToHeaderHash
  , protocolVersion
  , renderAbstractHash
  , slotLeaderHash
  , slotNumber
  , unAbstractHash
  , unAddressHash
  , unCryptoHash
  , unHeaderHash
  , unTxHash
  ) where

import           Bcc.Prelude hiding (catch)

import           Bcc.Binary (Raw)
import qualified Bcc.Crypto as Crypto
import qualified Bcc.Crypto.Wallet as Crypto

-- Import all 'bcc-ledger' functions and data types qualified so they do not
-- clash with the Bcc.Db functions and data types which are also imported
-- qualified.
import qualified Bcc.Chain.Block as Cole
import qualified Bcc.Chain.Common as Cole
import qualified Bcc.Chain.Genesis as Cole
import qualified Bcc.Chain.Slotting as Cole
import qualified Bcc.Chain.UTxO as Cole
import qualified Bcc.Chain.Update as Cole

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)
import qualified Data.Text.Encoding as Text


blockHash :: Cole.ABlock ByteString -> ByteString
blockHash = unHeaderHash . Cole.blockHashAnnotated

boundaryEpochNumber :: Cole.ABoundaryBlock ByteString -> Word64
boundaryEpochNumber = Cole.boundaryEpoch . Cole.boundaryHeader

blockNumber :: Cole.ABlock ByteString -> Word64
blockNumber =
  Cole.unChainDifficulty . Cole.headerDifficulty . Cole.blockHeader

blockPayload :: Cole.ABlock a -> [Cole.TxAux]
blockPayload =
  Cole.unTxPayload . Cole.bodyTxPayload . Cole.blockBody

blockPreviousHash :: Cole.ABlock a -> Cole.HeaderHash
blockPreviousHash = Cole.headerPrevHash . Cole.blockHeader

configSlotDuration :: Cole.Config -> Word64
configSlotDuration =
  fromIntegral . Cole.ppSlotDuration . Cole.gdProtocolParameters . Cole.configGenesisData

epochNumber :: Cole.ABlock ByteString -> Word64 -> Word64
epochNumber blk slotsPerEpoch =
  slotNumber blk `div` slotsPerEpoch

genesisToHeaderHash :: Cole.GenesisHash -> Cole.HeaderHash
genesisToHeaderHash = coerce

protocolVersion :: Cole.ABlock ByteString -> Cole.ProtocolVersion
protocolVersion = Cole.headerProtocolVersion . Cole.blockHeader

renderAbstractHash :: Crypto.AbstractHash algo a -> Text
renderAbstractHash =
    Text.decodeUtf8 . Base16.encode . Crypto.abstractHashToBytes

slotLeaderHash :: Cole.ABlock ByteString -> ByteString
slotLeaderHash =
  BS.take 28
    . Crypto.abstractHashToBytes . Crypto.hashRaw .LBS.fromStrict . Crypto.xpubPublicKey
    . Crypto.unVerificationKey . Cole.headerGenesisKey . Cole.blockHeader

slotNumber :: Cole.ABlock ByteString -> Word64
slotNumber =
  Cole.unSlotNumber . Cole.headerSlot . Cole.blockHeader

unAbstractHash :: Crypto.Hash Raw -> ByteString
unAbstractHash = Crypto.abstractHashToBytes

unAddressHash :: Cole.AddressHash Cole.Address' -> ByteString
unAddressHash = Crypto.abstractHashToBytes

unHeaderHash :: Cole.HeaderHash -> ByteString
unHeaderHash = Crypto.abstractHashToBytes

unTxHash :: Crypto.Hash Cole.Tx -> ByteString
unTxHash = Crypto.abstractHashToBytes

unCryptoHash :: Crypto.Hash Raw -> ByteString
unCryptoHash = Crypto.abstractHashToBytes
