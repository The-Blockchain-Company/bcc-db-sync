{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Godx.Sync.Config.Sophie
  ( SophieConfig (..)
  , readSophieGenesisConfig
  ) where

import qualified Godx.Crypto.Hash as Crypto

import           Godx.Db (textShow)

import           Godx.Sync.Config.Types
import           Godx.Sync.Error

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Shardagnostic.Consensus.Sophie.Eras (StandardSophie)
import           Shardagnostic.Consensus.Sophie.Ledger.Block ()
import           Shardagnostic.Consensus.Sophie.Node (SophieGenesis (..))

-- Everything in this file should be in shardagnostic-consensus so that both 'node' and 'db-=sync'
-- can use it.

data SophieConfig = SophieConfig
  { scConfig :: !(SophieGenesis StandardSophie)
  , scGenesisHash :: !GenesisHashSophie
  }

readSophieGenesisConfig
    :: SyncNodeConfig
    -> ExceptT SyncNodeError IO SophieConfig
readSophieGenesisConfig enc = do
  let file = unGenesisFile $ dncSophieGenesisFile enc
  firstExceptT (NESophieConfig file . renderSophieGenesisError)
    $ readGenesis (GenesisFile file) Nothing

-- -------------------------------------------------------------------------------------------------

data SophieGenesisError
     = GenesisReadError !FilePath !Text
     | GenesisHashMismatch !GenesisHashSophie !GenesisHashSophie -- actual, expected
     | GenesisDecodeError !FilePath !Text
     deriving Show

readGenesis
    :: GenesisFile -> Maybe GenesisHashSophie
    -> ExceptT SophieGenesisError IO SophieConfig
readGenesis (GenesisFile file) mbExpectedGenesisHash = do
    content <- handleIOExceptT (GenesisReadError file . textShow) $ BS.readFile file
    let genesisHash = GenesisHashSophie (Crypto.hashWith id content)
    checkExpectedGenesisHash genesisHash
    genesis <- firstExceptT (GenesisDecodeError file . Text.pack)
                  . hoistEither
                  $ Aeson.eitherDecodeStrict' content
    pure $ SophieConfig genesis genesisHash
  where
    checkExpectedGenesisHash :: GenesisHashSophie -> ExceptT SophieGenesisError IO ()
    checkExpectedGenesisHash actual =
      case mbExpectedGenesisHash of
        Just expected | actual /= expected
          -> left (GenesisHashMismatch actual expected)
        _ -> pure ()

renderSophieGenesisError :: SophieGenesisError -> Text
renderSophieGenesisError sge =
    case sge of
      GenesisReadError fp err ->
        mconcat
          [ "There was an error reading the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

      GenesisHashMismatch actual expected ->
        mconcat
          [ "Wrong Sophie genesis file: the actual hash is ", renderHash actual
          , ", but the expected Sophie genesis hash given in the node "
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
