{-# LANGUAGE OverloadedStrings #-}
module Bcc.Sync.Era.Bcc.Util
  ( unChainHash
  ) where

import           Bcc.Prelude

import qualified Data.ByteString.Short as BSS

import           Shardagnostic.Consensus.Bcc.Block (BccBlock)
import qualified Shardagnostic.Consensus.HardFork.Combinator as Consensus

import           Shardagnostic.Network.Block (ChainHash (..))


unChainHash :: ChainHash (BccBlock era) -> ByteString
unChainHash ch =
  case ch of
    GenesisHash -> "genesis"
    BlockHash bh -> BSS.fromShort (Consensus.getOneEraHash bh)


