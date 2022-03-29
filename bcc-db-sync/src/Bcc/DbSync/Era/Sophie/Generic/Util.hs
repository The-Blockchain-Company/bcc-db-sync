{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- Need this because both ghc-8.6.5 and ghc-8.10.2 incorrectly warns about a redundant constraint
-- in the definition of renderAddress.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Bcc.DbSync.Era.Sophie.Generic.Util
  ( annotateStakingCred
  , coinToDbEntropic
  , getPaymentCred
  , hasCredScript
  , getCredentialScriptHash
  , maybePaymentCred
  , mkSlotLeader
  , nonceToBytes
  , partitionMIRTargets
  , renderAddress
  , renderLanguageCostModel
  , renderRewardAcnt
  , serialiseRewardAcntWithNetwork
  , stakingCredHash
  , unitIntervalToDouble
  , unKeyHashRaw
  , unKeyHashView
  , unScriptHash
  , unTxHash
  ) where

import           Bcc.Prelude

import qualified Bcc.Api.Sophie as Api

import qualified Bcc.Crypto.Hash as Crypto

import           Bcc.Db (DbEntropic (..))
import qualified Bcc.Db as Db

import qualified Bcc.Ledger.Address as Ledger
import           Bcc.Ledger.Aurum.Language (Language)
import           Bcc.Ledger.Aurum.Scripts (CostModel (..))
import qualified Bcc.Ledger.BaseTypes as Ledger
import qualified Bcc.Ledger.Credential as Ledger
import qualified Bcc.Ledger.Keys as Ledger

import           Bcc.Ledger.Coin (Coin (..), DeltaCoin)
import qualified Bcc.Ledger.SafeHash as Ledger

import           Bcc.Sync.Util

import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as Text

import           Shardagnostic.Consensus.Bcc.Block (StandardAllegra, StandardCrypto, StandardJen,
                   StandardSophie)

import qualified Sophie.Spec.Ledger.Scripts as Sophie
import qualified Sophie.Spec.Ledger.Tx as Sophie
import qualified Sophie.Spec.Ledger.TxBody as Sophie


annotateStakingCred :: Ledger.Network -> Ledger.StakeCredential era -> Ledger.RewardAcnt era
annotateStakingCred = Sophie.RewardAcnt

coinToDbEntropic :: Coin -> DbEntropic
coinToDbEntropic = DbEntropic . fromIntegral . unCoin

getPaymentCred :: Ledger.Addr StandardCrypto -> Maybe (Ledger.PaymentCredential StandardCrypto)
getPaymentCred addr =
  case addr of
    Ledger.Addr _nw pcred _sref -> Just pcred
    Ledger.AddrBootstrap {} -> Nothing

hasCredScript :: Ledger.Credential kr StandardCrypto -> Bool
hasCredScript pc =
  case pc of
    Ledger.ScriptHashObj _ -> True
    Ledger.KeyHashObj {} -> False

maybePaymentCred :: Ledger.Addr era -> Maybe ByteString
maybePaymentCred addr =
  case addr of
    Ledger.Addr _nw pcred _sref ->
      Just $ LBS.toStrict (Binary.runPut $ Ledger.putCredential pcred)
    Ledger.AddrBootstrap {} ->
      Nothing

getCredentialScriptHash :: Ledger.Credential kr StandardCrypto -> Maybe ByteString
getCredentialScriptHash pc =
  case pc of
    Ledger.ScriptHashObj hash -> Just $ unScriptHash hash
    Ledger.KeyHashObj {} -> Nothing

mkSlotLeader :: ByteString -> Maybe Db.PoolHashId -> Db.SlotLeader
mkSlotLeader slHash mPoolId =
  let short = Text.decodeUtf8 (Base16.encode $ BS.take 8 slHash)
      slName = case mPoolId of
                Nothing -> "SophieGenesis-" <> short
                Just _ -> "Pool-" <> short
  in Db.SlotLeader slHash mPoolId slName

nonceToBytes :: Ledger.Nonce -> Maybe ByteString
nonceToBytes nonce =
  case nonce of
    Ledger.Nonce hash -> Just $ Crypto.hashToBytes hash
    Ledger.NeutralNonce -> Nothing

partitionMIRTargets
    :: [Sophie.MIRTarget StandardCrypto]
    -> ([Map (Ledger.Credential 'Ledger.Staking StandardCrypto) DeltaCoin], [Coin])
partitionMIRTargets =
    List.foldl' foldfunc ([], [])
  where
    foldfunc
        :: ([Map (Ledger.Credential 'Ledger.Staking StandardCrypto) DeltaCoin], [Coin])
        -> Sophie.MIRTarget StandardCrypto
        -> ([Map (Ledger.Credential 'Ledger.Staking StandardCrypto) DeltaCoin], [Coin])
    foldfunc (xs, ys) mt =
      case mt of
        Sophie.StakeAddressesMIR x -> (x : xs, ys)
        Sophie.SendToOppositePotMIR y -> (xs, y : ys)

type family LedgerEraToApiEra ledgerera where
  LedgerEraToApiEra StandardSophie = Api.SophieEra
  LedgerEraToApiEra StandardAllegra = Api.AllegraEra
  LedgerEraToApiEra StandardJen = Api.JenEra

renderAddress
    :: forall era ledgerera.
       LedgerEraToApiEra ledgerera ~ era
    => Api.SophieLedgerEra era ~ ledgerera
    => Api.IsSophieBasedEra era
    => ledgerera ~ StandardSophie
    => Ledger.Addr StandardCrypto -> Text
renderAddress addr = Api.serialiseAddress (Api.fromSophieAddr addr :: Api.AddressInEra era)

renderCostModel :: CostModel -> Text
renderCostModel (CostModel x) = textShow x

renderLanguageCostModel :: Map Language CostModel -> Text
renderLanguageCostModel mlc = textShow $ Map.map renderCostModel mlc

renderRewardAcnt :: Ledger.RewardAcnt StandardCrypto -> Text
renderRewardAcnt = Api.serialiseAddress . Api.fromSophieStakeAddr

-- Ignore the network in the `RewardAcnt` and use the provided one instead.
-- This is a workaround for https://github.com/The-Blockchain-Company/bcc-db-sync/issues/546
serialiseRewardAcntWithNetwork :: Ledger.Network -> Ledger.RewardAcnt StandardCrypto -> ByteString
serialiseRewardAcntWithNetwork network (Sophie.RewardAcnt _ cred) =
  Ledger.serialiseRewardAcnt $ Sophie.RewardAcnt network cred

stakingCredHash :: Ledger.Network -> Ledger.StakeCredential era -> ByteString
stakingCredHash network = Ledger.serialiseRewardAcnt . annotateStakingCred network

unitIntervalToDouble :: Ledger.UnitInterval -> Double
unitIntervalToDouble = fromRational . Ledger.unboundRational

unKeyHashRaw :: Ledger.KeyHash d era -> ByteString
unKeyHashRaw (Ledger.KeyHash kh) = Crypto.hashToBytes kh

unKeyHashView :: Ledger.KeyHash 'Ledger.StakePool StandardCrypto -> Text
unKeyHashView = Api.serialiseToBech32 . Api.StakePoolKeyHash

unScriptHash :: Sophie.ScriptHash StandardCrypto -> ByteString
unScriptHash (Sophie.ScriptHash h) = Crypto.hashToBytes h

unTxHash :: Sophie.TxId era -> ByteString
unTxHash (Sophie.TxId txid) = Crypto.hashToBytes $ Ledger.extractHash txid
