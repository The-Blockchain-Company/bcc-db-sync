{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Bcc.Db.Tool.Validate.Balance
  ( ledgerAddrBalance
  ) where

import qualified Bcc.Api.Sophie as Api

import qualified Bcc.Chain.Block as Cole
import           Bcc.Chain.Common (CompactAddress, Entropic, decodeAddressBase58, sumEntropic,
                   toCompactAddress, unsafeGetEntropic)
import qualified Bcc.Chain.UTxO as Cole

import           Bcc.Ledger.Address (BootstrapAddress (..))
import qualified Bcc.Ledger.Aurum.TxBody as Aurum
import qualified Bcc.Ledger.Core as Ledger
import           Bcc.Ledger.Era (Crypto)

import           Bcc.Ledger.Compactible
import           Bcc.Ledger.Val
import           Bcc.Prelude

import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import           Shardagnostic.Consensus.Cole.Ledger
import           Shardagnostic.Consensus.Bcc.Block (BccBlock, LedgerState (..), StandardCrypto)
import           Shardagnostic.Consensus.Sophie.Ledger.Block (SophieBlock)
import           Shardagnostic.Consensus.Sophie.Ledger.Ledger

import           Sophie.Spec.Ledger.CompactAddr (CompactAddr, compactAddr)
import qualified Sophie.Spec.Ledger.LedgerState as Sophie
import qualified Sophie.Spec.Ledger.TxBody as Sophie
import qualified Sophie.Spec.Ledger.UTxO as Sophie

import           Sophie.Spec.Ledger.API (Addr (..), Coin (..))

-- Given an address, return it's current UTxO balance.
ledgerAddrBalance :: Text -> LedgerState (BccBlock StandardCrypto) -> Either Text Word64
ledgerAddrBalance addr lsc =
    case lsc of
      LedgerStateCole st -> getColeBalance addr $ Cole.cvsUtxo $ coleLedgerState st
      LedgerStateSophie st -> getSophieBalance addr $ getUTxO st
      LedgerStateAllegra st -> getSophieBalance addr $ getUTxO st
      LedgerStateJen st -> getSophieBalance addr $ getUTxO st
      LedgerStateAurum st -> getAurumBalance addr $ getUTxO st
  where
    getUTxO :: LedgerState (SophieBlock era) -> Sophie.UTxO era
    getUTxO = Sophie._utxo . Sophie._utxoState . Sophie.esLState . Sophie.nesEs . sophieLedgerState

getColeBalance :: Text -> Cole.UTxO -> Either Text Word64
getColeBalance addrText utxo = do
    case toCompactAddress <$> decodeAddressBase58 addrText of
      Left err -> Left $ textShow err
      Right caddr -> bimap show unsafeGetEntropic . sumEntropic . mapMaybe (compactTxOutValue caddr) . Map.elems $ Cole.unUTxO utxo
  where
    compactTxOutValue :: CompactAddress -> Cole.CompactTxOut -> Maybe Entropic
    compactTxOutValue caddr (Cole.CompactTxOut bcaddr entropic) =
      if caddr == bcaddr
        then Just entropic
        else Nothing

getSophieBalance
    :: forall era. Ledger.TxOut era ~ Sophie.TxOut era -- somewhere in ledger-spec, there is probably a better constraint synonym for these
    => Compactible (Ledger.Value era) => Val (Ledger.Value era)
    => Text -> Sophie.UTxO era -> Either Text Word64
getSophieBalance addrText utxo = do
    caddr <- getCompactAddress addrText
    Right . fromIntegral . sum $ unCoin <$> mapMaybe (compactTxOutValue caddr) (Map.elems $ Sophie.unUTxO utxo)
  where
    compactTxOutValue :: CompactAddr (Crypto era) -> Ledger.TxOut era -> Maybe Coin
    compactTxOutValue caddr (Sophie.TxOutCompact scaddr v) =
      if caddr == scaddr
        then Just $ coin (fromCompact v)
        else Nothing

getAurumBalance
    :: forall era. Ledger.TxOut era ~ Aurum.TxOut era -- somewhere in ledger-spec, there is probably a better constraint synonym for these
    => Compactible (Ledger.Value era) => Val (Ledger.Value era)
    => Text -> Sophie.UTxO era -> Either Text Word64
getAurumBalance addrText utxo = do
    caddr <- getCompactAddress addrText
    Right . fromIntegral . sum $ unCoin <$> mapMaybe (compactTxOutValue caddr) (Map.elems $ Sophie.unUTxO utxo)
  where
    compactTxOutValue :: CompactAddr (Crypto era) -> Ledger.TxOut era -> Maybe Coin
    compactTxOutValue caddr txOut =
      let (scaddr, val) = case txOut of
                            Aurum.TxOutCompact a v -> (a, v)
                            Aurum.TxOutCompactDH a v _ -> (a, v)
      in if caddr == scaddr
          then Just $ coin (fromCompact val)
          else Nothing

getCompactAddress :: Text -> Either Text (CompactAddr c)
getCompactAddress addrText = case Api.deserialiseAddress (Api.AsAddress Api.AsSophieAddr) addrText of
    Nothing ->
      case decodeAddressBase58 addrText of
        Left err -> Left $ textShow err
        Right badrr -> Right $ compactAddr (AddrBootstrap $ BootstrapAddress badrr)
    Just (Api.SophieAddress n p s) ->
      let addr = Addr n (coerce p) (coerce s)
      in Right $ compactAddr addr

textShow :: Show a => a -> Text
textShow = Text.pack . show
