{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Bcc.Sync.Error
  ( SyncInvariant (..)
  , SyncNodeError (..)
  , annotateInvariantTx
  , bsBase16Encode
  , dbSyncNodeError
  , dbSyncInvariant
  , renderSyncInvariant
  , renderSyncNodeError
  ) where

import           Bcc.Prelude

import qualified Bcc.Chain.Genesis as Cole
import qualified Bcc.Chain.UTxO as Cole
import qualified Bcc.Crypto as Crypto (serializeCborHash)

import           Control.Monad.Trans.Except.Extra (left)

import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Bcc.Sync.Era.Cole.Util as Cole
import           Bcc.Sync.Util

data SyncInvariant
  = EInvInOut !Word64 !Word64
  | EInvTxInOut !Cole.Tx !Word64 !Word64

data SyncNodeError
  = NEError !Text
  | NEInvariant !Text !SyncInvariant
  | NEBlockMismatch !Word64 !ByteString !ByteString
  | NEColeConfig !FilePath !Cole.ConfigurationError
  | NESophieConfig !FilePath !Text
  | NEAurumConfig !FilePath !Text
  | NEBccConfig !Text

annotateInvariantTx :: Cole.Tx -> SyncInvariant -> SyncInvariant
annotateInvariantTx tx ei =
  case ei of
    EInvInOut inval outval -> EInvTxInOut tx inval outval
    _other -> ei

dbSyncNodeError :: Monad m => Text -> ExceptT SyncNodeError m a
dbSyncNodeError = left . NEError

dbSyncInvariant :: Monad m => Text -> SyncInvariant -> ExceptT SyncNodeError m a
dbSyncInvariant loc = left . NEInvariant loc

renderSyncInvariant :: SyncInvariant -> Text
renderSyncInvariant ei =
  case ei of
    EInvInOut inval outval ->
      mconcat [ "input value ", textShow inval, " < output value ", textShow outval ]
    EInvTxInOut tx inval outval ->
      mconcat
        [ "tx ", bsBase16Encode (Cole.unTxHash $ Crypto.serializeCborHash tx)
        , " : input value ", textShow inval, " < output value ", textShow outval
        , "\n", textShow tx
        ]

renderSyncNodeError :: SyncNodeError -> Text
renderSyncNodeError ne =
  case ne of
    NEError t -> "Error: " <> t
    NEInvariant loc i -> mconcat [ loc, ": " <> renderSyncInvariant i ]
    NEBlockMismatch blkNo hashDb hashBlk ->
      mconcat
        [ "Block mismatch for block number ", textShow blkNo, ", db has "
        , bsBase16Encode hashDb, " but chain provided ", bsBase16Encode hashBlk
        ]
    NEColeConfig fp ce ->
      mconcat
        [ "Failed reading Cole genesis file ", textShow fp, ": ", textShow ce
        ]
    NESophieConfig fp txt ->
      mconcat
        [ "Failed reading Sophie genesis file ", textShow fp, ": ", txt
        ]
    NEAurumConfig fp txt ->
      mconcat
        [ "Failed reading Aurum genesis file ", textShow fp, ": ", txt
        ]
    NEBccConfig err ->
      mconcat
        [ "With Bcc protocol, Cole/Sophie config mismatch:\n"
        , "   ", err
        ]

bsBase16Encode :: ByteString -> Text
bsBase16Encode bs =
  case Text.decodeUtf8' (Base16.encode bs) of
    Left _ -> Text.pack $ "UTF-8 decode failed for " ++ show bs
    Right txt -> txt
