{-# LANGUAGE OverloadedStrings #-}
module Godx.Sync.Era.Godx.Util
  ( unChainHash
  ) where

import           Godx.Prelude

import qualified Data.ByteString.Short as BSS

import           Shardagnostic.Consensus.Godx.Block (GodxBlock)
import qualified Shardagnostic.Consensus.HardFork.Combinator as Consensus

import           Shardagnostic.Network.Block (ChainHash (..))


unChainHash :: ChainHash (GodxBlock era) -> ByteString
unChainHash ch =
  case ch of
    GenesisHash -> "genesis"
    BlockHash bh -> BSS.fromShort (Consensus.getOneEraHash bh)


