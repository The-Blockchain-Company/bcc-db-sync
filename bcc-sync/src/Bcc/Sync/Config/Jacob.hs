{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Bcc.Sync.Config.Aurum
  ( readAurumGenesisConfig
  ) where


import qualified Bcc.Crypto.Hash as Crypto

import           Bcc.Db (textShow)

import           Bcc.Sync.Config.Types
import           Bcc.Sync.Error

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Bcc.Api.Orphans ()
import           Bcc.Ledger.Aurum.Genesis

readAurumGenesisConfig
    :: SyncNodeConfig
    -> ExceptT SyncNodeError IO AurumGenesis
readAurumGenesisConfig enc = do
    let file = unGenesisFile $ dncAurumGenesisFile enc
    firstExceptT (NEAurumConfig file . renderAurumGenesisError) $
        readGenesis (GenesisFile file) Nothing

data AurumGenesisError
  = GenesisReadError !FilePath !Text
  | GenesisHashMismatch !GenesisHashSophie !GenesisHashSophie -- actual, expected
  | GenesisDecodeError !FilePath !Text
  deriving Show

readGenesis
    :: GenesisFile -> Maybe GenesisHashSophie
    -> ExceptT AurumGenesisError IO AurumGenesis
readGenesis (GenesisFile file) mbExpectedGenesisHash = do
    content <- handleIOExceptT (GenesisReadError file . textShow) $ BS.readFile file
    let genesisHash = GenesisHashSophie (Crypto.hashWith id content)
    checkExpectedGenesisHash genesisHash
    firstExceptT (GenesisDecodeError file . Text.pack)
        . hoistEither
        $ Aeson.eitherDecodeStrict' content
  where
    checkExpectedGenesisHash :: GenesisHashSophie -> ExceptT AurumGenesisError IO ()
    checkExpectedGenesisHash actual =
      case mbExpectedGenesisHash of
        Just expected | actual /= expected
          -> left (GenesisHashMismatch actual expected)
        _ -> pure ()

renderAurumGenesisError :: AurumGenesisError -> Text
renderAurumGenesisError age =
    case age of
      GenesisReadError fp err ->
        mconcat
          [ "There was an error reading the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

      GenesisHashMismatch actual expected ->
        mconcat
          [ "Wrong Aurum genesis file: the actual hash is ", renderHash actual
          , ", but the expected Aurum genesis hash given in the node "
          , "configuration file is ", renderHash expected, "."
          ]

      GenesisDecodeError fp err ->
        mconcat
          [ "There was an error parsing the genesis file: ", Text.pack fp
          , " Error: ", err
          ]
  where
    renderHash :: GenesisHashSophie -> Text
    renderHash (GenesisHashSophie h) = Text.decodeUtf8 $ Base16.encode (Crypto.hashToBytes h)
