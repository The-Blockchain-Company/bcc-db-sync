{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Bcc.Sync.Tracing.ToObjectOrphans () where

import           Data.Aeson ((.=))
import           Data.Text (Text)
import qualified Data.Text as Text

import           Bcc.BM.Data.Tracer
import           Bcc.Tracing.OrphanInstances.Network ()

import           Shardagnostic.Consensus.Cole.Ledger.Block (ColeBlock)
import           Shardagnostic.Network.Block (Point (..), Tip (..))
import           Shardagnostic.Network.NodeToNode (TraceSendRecv (..))
import           Shardagnostic.Network.Protocol.ChainSync.Type (ChainSync)


instance HasTextFormatter (TraceSendRecv (ChainSync blk (Point blk) (Tip blk))) where
  formatText _ = Text.pack . show

instance ToObject ColeBlock where
  toObject _verb msg =
    mkObject [ "kind" .= ("ColeBlock" :: String)
             , "event" .= show msg
             ]

instance Transformable Text IO (TraceSendRecv (ChainSync blk (Point blk) (Tip blk))) where
  trTransformer = trStructuredText
