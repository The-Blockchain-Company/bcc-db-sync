{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Bcc.DbSync.Era.Sophie.Generic.Tx
  ( Tx (..)
  , TxCertificate (..)
  , TxIn (..)
  , TxOut (..)
  , TxRedeemer (..)
  , TxWithdrawal (..)
  , TxScript (..)
  , fromSophieTx
  , fromAllegraTx
  , fromJenTx
  , fromAurumTx
  ) where

import           Bcc.Prelude

import           Bcc.Api.Sophie (TxMetadataValue (..))

import qualified Bcc.Crypto.Hash as Crypto

import           Bcc.DbSync.Era.Sophie.Generic.Metadata
import           Bcc.DbSync.Era.Sophie.Generic.ParamProposal
import           Bcc.DbSync.Era.Sophie.Generic.Util
import           Bcc.DbSync.Era.Sophie.Generic.Witness

import qualified Bcc.Ledger.Address as Ledger
import           Bcc.Ledger.Aurum (AurumEra)
import qualified Bcc.Ledger.Aurum.Data as Aurum
import qualified Bcc.Ledger.Aurum.PParams as Aurum
import           Bcc.Ledger.Aurum.Scripts (ExUnits (..), Script (..), Tag (..), txscriptfee)
import           Bcc.Ledger.Aurum.Tx (ValidatedTx (..))
import qualified Bcc.Ledger.Aurum.Tx as Aurum
import qualified Bcc.Ledger.Aurum.TxBody as Aurum
import qualified Bcc.Ledger.Aurum.TxWitness as Ledger
import           Bcc.Ledger.Coin (Coin (..))
import qualified Bcc.Ledger.Core as Ledger
import qualified Bcc.Ledger.Era as Ledger
import           Bcc.Ledger.Jen.Value (AssetName, PolicyID, Value (..))
import qualified Bcc.Ledger.SafeHash as Ledger
import qualified Bcc.Ledger.SophieMA.AuxiliaryData as SophieMa
import qualified Bcc.Ledger.SophieMA.TxBody as SophieMa

import           Bcc.Slotting.Slot (SlotNo (..))

import qualified Data.ByteString.Short as SBS
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Maybe.Strict (strictMaybeToMaybe)
import           Data.MemoBytes (MemoBytes (..))
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Set as Set

import           Shardagnostic.Consensus.Bcc.Block (StandardAllegra, StandardAurum, StandardCrypto,
                   StandardJen, StandardSophie)
import           Shardagnostic.Consensus.Sophie.Ledger.Block (SophieBasedEra)

import           Sophie.Spec.Ledger.Scripts (ScriptHash)
import qualified Sophie.Spec.Ledger.Tx as Sophie
import qualified Sophie.Spec.Ledger.TxBody as Sophie

data Tx = Tx
  { txHash :: !ByteString
  , txBlockIndex :: !Word64
  , txSize :: !Word64
  , txValidContract :: !Bool
  , txInputs :: ![TxIn]
  , txCollateralInputs :: ![TxIn]
  , txOutputs :: ![TxOut]
  , txFees :: !Coin
  , txOutSum :: !Coin
  , txInvalidBefore :: !(Maybe SlotNo)
  , txInvalidHereafter :: !(Maybe SlotNo)
  , txWithdrawalSum :: !Coin
  , txMetadata :: !(Maybe (Map Word64 TxMetadataValue))
  , txCertificates :: ![TxCertificate]
  , txWithdrawals :: ![TxWithdrawal]
  , txParamProposal :: ![ParamProposal]
  , txMint :: !(Value StandardCrypto)
  , txRedeemer :: [TxRedeemer]
  , txScriptSizes :: [Word64] -- this contains only the sizes of zerepoch scripts in witnesses
  , txScripts :: [TxScript]
  , txScriptsFee :: Coin
  }

data TxCertificate = TxCertificate
  { txcRedeemerIndex :: !(Maybe Word64)
  , txcIndex :: !Word16
  , txcCert :: !(Sophie.DCert StandardCrypto)
  }

data TxWithdrawal = TxWithdrawal
  { txwRedeemerIndex :: !(Maybe Word64)
  , txwRewardAccount :: !(Sophie.RewardAcnt StandardCrypto)
  , txwAmount :: !Coin
  }

data TxIn = TxIn
  { txInHash :: !ByteString
  , txInIndex :: !Word16
  , txInRedeemerIndex :: !(Maybe Word64) -- This only has a meaning for Aurum.
  }

data TxOut = TxOut
  { txOutIndex :: !Word16
  , txOutAddress :: !(Ledger.Addr StandardCrypto)
  , txOutBccValue :: !Coin
  , txOutMaValue :: !(Map (PolicyID StandardCrypto) (Map AssetName Integer))
  , txOutDataHash :: !(Maybe ByteString)
  }

data TxRedeemer = TxRedeemer
  { txRedeemerMem :: !Word64
  , txRedeemerSteps :: !Word64
  , txRedeemerPurpose :: !Tag
  , txRedeemerFee :: !Coin
  , txRedeemerIndex :: !Word64
  , txRedeemerScriptHash :: Maybe (Either TxIn ByteString)
  }

data TxScript = TxScript
  { txScriptHash :: !ByteString
  , txScriptZerepochSize :: Maybe Word64
  }

fromAllegraTx :: (Word64, Sophie.Tx StandardAllegra) -> Tx
fromAllegraTx (blkIndex, tx) =
    Tx
      { txHash = txHashId tx
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ getField @"txsize" tx
      , txValidContract = True
      , txInputs = map (fromTxIn Nothing) (toList $ SophieMa.inputs rawTxBody)
      , txCollateralInputs = [] -- Allegra does not have collateral inputs
      , txOutputs = zipWith fromTxOut [0 .. ] $ toList (SophieMa.outputs rawTxBody)
      , txFees = SophieMa.txfee rawTxBody
      , txOutSum = Coin . sum $ map txOutValue (SophieMa.outputs rawTxBody)
      , txInvalidBefore = strictMaybeToMaybe . SophieMa.invalidBefore $ SophieMa.vldt rawTxBody
      , txInvalidHereafter = strictMaybeToMaybe . SophieMa.invalidHereafter $ SophieMa.vldt rawTxBody
      , txWithdrawalSum = Coin . sum . map unCoin . Map.elems
                            . Sophie.unWdrl $ SophieMa.wdrls rawTxBody
      , txMetadata = fromAllegraMetadata <$> txMeta tx
      , txCertificates = zipWith (TxCertificate Nothing) [0..] (map coerceCertificate . toList $ SophieMa.certs rawTxBody)
      , txWithdrawals = map (mkTxWithdrawal Nothing) (Map.toList . Sophie.unWdrl $ SophieMa.wdrls rawTxBody)
      , txParamProposal = maybe [] (convertParamProposal (Allegra Standard)) $ strictMaybeToMaybe (SophieMa.update rawTxBody)
      , txMint = mempty     -- Allegra does not support Multi-Assets
      , txRedeemer = []     -- Allegra does not support Redeemers
      , txScriptSizes = []    -- Allegra does not support scripts
      , txScripts = []        -- We don't populate scripts for Allegra
      , txScriptsFee = Coin 0 -- Allegra does not support scripts
      }
  where
    fromTxOut :: Word16 -> Sophie.TxOut StandardAllegra -> TxOut
    fromTxOut index (Sophie.TxOut addr bcc) =
      TxOut
        { txOutIndex = index
        , txOutAddress = coerceAddress addr
        , txOutBccValue = bcc
        , txOutMaValue = mempty -- Allegra does not support Multi-Assets
        , txOutDataHash = mempty -- Allegra does not support scripts
        }

    txMeta :: Sophie.Tx StandardAllegra -> Maybe (SophieMa.AuxiliaryData StandardAllegra)
    txMeta (Sophie.Tx _body _wit md) = strictMaybeToMaybe md

    txOutValue :: Sophie.TxOut StandardAllegra -> Integer
    txOutValue (Sophie.TxOut _ (Coin coin)) = coin

    rawTxBody :: SophieMa.TxBodyRaw StandardAllegra
    rawTxBody =
      case tx of
        (Sophie.Tx (SophieMa.TxBodyConstr txBody) _wit _md) -> memotype txBody


fromSophieTx :: (Word64, Sophie.Tx StandardSophie) -> Tx
fromSophieTx (blkIndex, tx) =
    Tx
      { txHash = txHashId tx
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ getField @"txsize" tx
      , txValidContract = True
      , txInputs = map (fromTxIn Nothing) (toList . Sophie._inputs $ Sophie.body tx)
      , txCollateralInputs = [] -- Sophie does not have collateral inputs
      , txOutputs = zipWith fromTxOut [0 .. ] $ toList (Sophie._outputs $ Sophie.body tx)
      , txFees = Sophie._txfee (Sophie.body tx)
      , txOutSum = Coin . sum $ map txOutValue (Sophie._outputs $ Sophie.body tx)
      , txInvalidBefore = Nothing
      , txInvalidHereafter = Just $ Sophie._ttl (Sophie.body tx)
      , txWithdrawalSum = Coin . sum . map unCoin . Map.elems
                            . Sophie.unWdrl $ Sophie._wdrls (Sophie.body tx)
      , txMetadata = fromSophieMetadata <$> strictMaybeToMaybe (getField @"auxiliaryData" tx)
      , txCertificates = zipWith (TxCertificate Nothing) [0..] (toList . Sophie._certs $ Sophie.body tx)
      , txWithdrawals = map (mkTxWithdrawal Nothing) (Map.toList . Sophie.unWdrl . Sophie._wdrls $ Sophie.body tx)
      , txParamProposal = maybe [] (convertParamProposal (Sophie Standard)) $ strictMaybeToMaybe (Sophie._txUpdate $ Sophie.body tx)
      , txMint = mempty     -- Sophie does not support Multi-Assets
      , txRedeemer = []     -- Sophie does not support Redeemer
      , txScriptSizes = []    -- Sophie does not support scripts
      , txScripts = []        -- We don't populate scripts for Sophie
      , txScriptsFee = Coin 0 -- Sophie does not support scripts
      }
  where
    fromTxOut :: Word16 -> Sophie.TxOut StandardSophie -> TxOut
    fromTxOut index (Sophie.TxOut addr bcc) =
      TxOut
        { txOutIndex = index
        , txOutAddress = coerceAddress addr
        , txOutBccValue = bcc
        , txOutMaValue = mempty -- Sophie does not support Multi-Assets
        , txOutDataHash = mempty -- Sophie does not support scripts
        }

    txOutValue :: Sophie.TxOut StandardSophie -> Integer
    txOutValue (Sophie.TxOut _ (Coin coin)) = coin

fromJenTx :: (Word64, Sophie.Tx StandardJen) -> Tx
fromJenTx (blkIndex, tx) =
    Tx
      { txHash = txHashId tx
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ getField @"txsize" tx
      , txValidContract = True
      , txInputs = map (fromTxIn Nothing) (toList . SophieMa.inputs $ unTxBodyRaw tx)
      , txCollateralInputs = [] -- Jen does not have collateral inputs
      , txOutputs = zipWith fromTxOut [0 .. ] $ toList (SophieMa.outputs $ unTxBodyRaw tx)
      , txFees = SophieMa.txfee (unTxBodyRaw tx)
      , txOutSum = Coin . sum $ map txOutValue (SophieMa.outputs $ unTxBodyRaw tx)
      , txInvalidBefore = strictMaybeToMaybe . SophieMa.invalidBefore $ SophieMa.vldt (unTxBodyRaw tx)
      , txInvalidHereafter = strictMaybeToMaybe . SophieMa.invalidHereafter $ SophieMa.vldt (unTxBodyRaw tx)
      , txWithdrawalSum = Coin . sum . map unCoin . Map.elems
                            . Sophie.unWdrl $ SophieMa.wdrls (unTxBodyRaw tx)
      , txMetadata = fromJenMetadata <$> txMeta tx
      , txCertificates = zipWith (TxCertificate Nothing) [0..] (map coerceCertificate . toList . SophieMa.certs $ unTxBodyRaw tx)
      , txWithdrawals = map (mkTxWithdrawal Nothing) (Map.toList . Sophie.unWdrl . SophieMa.wdrls $ unTxBodyRaw tx)
      , txParamProposal = maybe [] (convertParamProposal (Jen Standard)) $ strictMaybeToMaybe (SophieMa.update $ unTxBodyRaw tx)
      , txMint = coerceMint (SophieMa.mint $ unTxBodyRaw tx)
      , txRedeemer = []     -- Jen does not support Redeemer
      , txScriptSizes = []    -- Jen does not support scripts
      , txScripts = []        -- We don't populate scripts for Jen
      , txScriptsFee = Coin 0 -- Jen does not support scripts
      }
  where
    fromTxOut :: Word16 -> Sophie.TxOut StandardJen -> TxOut
    fromTxOut index (Sophie.TxOut addr (Value bcc maMap)) =
      TxOut
        { txOutIndex = index
        , txOutAddress = coerceAddress addr
        , txOutBccValue = Coin bcc
        , txOutMaValue = coerceMultiAsset maMap
        , txOutDataHash = mempty -- Jen does not support scripts
        }

    txMeta :: Sophie.Tx StandardJen -> Maybe (SophieMa.AuxiliaryData StandardJen)
    txMeta (Sophie.Tx _body _wit md) = strictMaybeToMaybe md

    txOutValue :: Sophie.TxOut StandardJen -> Integer
    txOutValue (Sophie.TxOut _ (Value coin _ma)) = coin

    unTxBodyRaw :: Sophie.Tx StandardJen -> SophieMa.TxBodyRaw StandardJen
    unTxBodyRaw (Sophie.Tx (SophieMa.TxBodyConstr txBody) _wit _md) = memotype txBody

fromAurumTx :: Ledger.PParams StandardAurum -> (Word64, Ledger.Tx StandardAurum) -> Tx
fromAurumTx pp (blkIndex, tx) =
    Tx
      { txHash = Crypto.hashToBytes . Ledger.extractHash $ Ledger.hashAnnotated txBody
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ getField @"txsize" tx
      , txValidContract = isValid2
      , txInputs = txIns
      , txCollateralInputs = map (fromTxIn Nothing) . toList $ getField @"collateral" txBody
      , txOutputs = zipWith fromTxOut [0 .. ] . toList $ getField @"outputs" txBody
      , txFees = getField @"txfee" txBody
      , txOutSum = Coin . sum $ map txOutValue (getField @"outputs" txBody)
      , txInvalidBefore = strictMaybeToMaybe . SophieMa.invalidBefore $ getField @"vldt" txBody
      , txInvalidHereafter = strictMaybeToMaybe . SophieMa.invalidHereafter $ getField @"vldt" txBody
      , txWithdrawalSum = Coin . sum . map unCoin . Map.elems
                            . Sophie.unWdrl $ getField @"wdrls" txBody
      , txMetadata = fromAurumMetadata <$> strictMaybeToMaybe (getField @"auxiliaryData" tx)
      , txCertificates = txCerts
      , txWithdrawals = txWdrls
      , txParamProposal = maybe [] (convertParamProposal (Aurum Standard)) $ strictMaybeToMaybe (getField @"update" txBody)
      , txMint = coerceMint (getField @"mint" txBody)
      , txRedeemer = redeemers
      , txScriptSizes = sizes
      , txScripts = scripts
      , txScriptsFee = minFees
      }
  where
    fromTxOut :: Word16 -> Aurum.TxOut StandardAurum -> TxOut
    fromTxOut index (Aurum.TxOut addr (Value bcc maMap) mDataHash) =
      TxOut
        { txOutIndex = index
        , txOutAddress = coerceAddress addr
        , txOutBccValue = Coin bcc
        , txOutMaValue = coerceMultiAsset maMap
        , txOutDataHash = getDataHash mDataHash
        }

    txBody :: Ledger.TxBody StandardAurum
    txBody = getField @"body" tx

    sizes :: [Word64]
    sizes = mapMaybe getScriptSize $ toList $ Ledger.txscripts $ getField @"wits" tx

    scripts :: [TxScript]
    scripts =
      mkTxScript
        <$> (Map.toList (getField @"txscripts" $ getField @"wits" tx)
            ++ getAuxScripts (getField @"auxiliaryData" tx))

    getAuxScripts
        :: SophieMa.StrictMaybe (Aurum.AuxiliaryData StandardAurum)
        -> [(ScriptHash StandardCrypto, Script (AurumEra StandardCrypto))]
    getAuxScripts maux =
      case strictMaybeToMaybe maux of
        Nothing -> []
        Just (Aurum.AuxiliaryData _ scrs) ->
          map (\scr -> (Ledger.hashScript @StandardAurum scr, scr)) $ toList scrs

    getDataHash :: SophieMa.StrictMaybe (Ledger.SafeHash crypto a) -> Maybe ByteString
    getDataHash mDataHash =
      case strictMaybeToMaybe mDataHash of
        Nothing -> Nothing
        Just dataHash -> Just $ Crypto.hashToBytes (Ledger.extractHash dataHash)

    mkTxScript :: (ScriptHash StandardCrypto, Script (AurumEra StandardCrypto)) -> TxScript
    mkTxScript (hsh, script) = TxScript
      { txScriptHash = unScriptHash hsh
      , txScriptZerepochSize = getScriptSize script
      }

    getScriptSize :: Script (AurumEra StandardCrypto) -> Maybe Word64
    getScriptSize (TimelockScript _) = Nothing
    getScriptSize (ZerepochScript sbs) = Just $ fromIntegral $ SBS.length sbs

    minFees :: Coin
    minFees = txscriptfee (Aurum._prices pp) $ Aurum.totExUnits tx

    txOutValue :: Aurum.TxOut StandardAurum -> Integer
    txOutValue (Aurum.TxOut _addr (Value coin _ma) _dataHash) = coin

    certificates :: StrictSeq (Sophie.DCert StandardCrypto)
    certificates = getField @"certs" txBody

    mkTxCertificate :: Word16 -> Sophie.DCert StandardCrypto -> TxCertificate
    mkTxCertificate n cert =
      TxCertificate
        { txcIndex = n
        , txcRedeemerIndex = strictMaybeToMaybe $ Aurum.indexOf cert certificates
        , txcCert = cert
        }

    txCerts :: [TxCertificate]
    txCerts = zipWith mkTxCertificate [0..] (map coerceCertificate . toList $ certificates)

    withdrawals :: Map (Sophie.RewardAcnt StandardCrypto) Coin
    withdrawals = Sophie.unWdrl $ getField @"wdrls" txBody

    mkTxWithdrawal' :: (Sophie.RewardAcnt era, Coin) -> TxWithdrawal
    mkTxWithdrawal' (acnt, coin) =
      let acnt' = coerce acnt
      in mkTxWithdrawal (strictMaybeToMaybe $ Aurum.indexOf acnt' withdrawals) (acnt', coin)

    txWdrls :: [TxWithdrawal]
    txWdrls = map mkTxWithdrawal' (Map.toList withdrawals)

    -- This is true if second stage contract validation passes.
    isValid2 :: Bool
    isValid2 =
      case Aurum.isValid tx of
        Aurum.IsValid x -> x

    txIns :: [TxIn]
    txIns =
      if isValid2 then
        let inputsSet = getField @"inputs" txBody
            withIndex txIn = fromTxIn (strictMaybeToMaybe $ Aurum.indexOf txIn inputsSet) txIn
        in map withIndex $ toList inputsSet
      else
          let inputsSet = getField @"collateral" txBody
          in map (fromTxIn Nothing) $ toList inputsSet

    redeemers :: [TxRedeemer]
    redeemers =
      mkRedeemer <$> Map.toList (snd <$> Ledger.unRedeemers (getField @"txrdmrs" (getField @"wits" tx)))

    mkRedeemer :: (Ledger.RdmrPtr, ExUnits) -> TxRedeemer
    mkRedeemer (ptr@(Ledger.RdmrPtr tag index), exUnits) = TxRedeemer
      { txRedeemerMem = exUnitsMem exUnits
      , txRedeemerSteps = exUnitsSteps exUnits
      , txRedeemerFee = txscriptfee (Aurum._prices pp) exUnits
      , txRedeemerPurpose = tag
      , txRedeemerIndex = index
      , txRedeemerScriptHash = findScriptHash ptr
      }

    -- For 'Spend' script, we need to resolve the 'TxIn' to find the ScriptHash
    -- so we return 'Left TxIn' and resolve it later from the db. In other cases
    -- we can directly find the 'ScriptHash'.
    findScriptHash :: Ledger.RdmrPtr -> Maybe (Either TxIn ByteString)
    findScriptHash (Ledger.RdmrPtr tag index) =
      case tag of
        Spend ->
          -- We always use the real inputs here, instead of the collateral, because
          -- this just helps us find the script hash later, by resolving the input.
          Left . fromTxIn (Just index) <$> elemAtSet index (getField @"inputs" txBody)
        Rewrd ->
          Right <$> (scriptHashAcnt . txwRewardAccount =<< find (\wdrl -> txwRedeemerIndex wdrl == Just index) txWdrls)
        Cert ->
          Right <$> (scriptHashCert . txcCert =<< find (\cert -> txcRedeemerIndex cert == Just index) txCerts)
        Mint ->
          Right . unScriptHash <$> elemAtSet index (getField @"minted" txBody)

-- -------------------------------------------------------------------------------------------------

-- Coerce is safe here because 'era' is a phantom type.
coerceAddress :: Ledger.Addr era -> Ledger.Addr StandardCrypto
coerceAddress saddr =
  case saddr of
    Ledger.Addr nw pcred sref -> Ledger.Addr nw (coerce pcred) (coerce sref)
    Ledger.AddrBootstrap addr -> Ledger.AddrBootstrap (coerce addr)

coerceCertificate :: Sophie.DCert era -> Sophie.DCert StandardCrypto
coerceCertificate cert =
  case cert of
    Sophie.DCertDeleg deleg -> Sophie.DCertDeleg (coerce deleg)
    Sophie.DCertPool pool -> Sophie.DCertPool (coercePoolCert pool)
    Sophie.DCertMir (Sophie.MIRCert pot target) -> Sophie.DCertMir (Sophie.MIRCert pot (coerceMIRTarget target))
    Sophie.DCertGenesis gen -> Sophie.DCertGenesis (coerce gen)

coerceMIRTarget :: Sophie.MIRTarget crypto -> Sophie.MIRTarget StandardCrypto
coerceMIRTarget mt =
  case mt of
    Sophie.StakeAddressesMIR m -> Sophie.StakeAddressesMIR (Map.mapKeys coerce m)
    Sophie.SendToOppositePotMIR c -> Sophie.SendToOppositePotMIR c

coerceMint :: Value era -> Value StandardCrypto
coerceMint (Value bcc maMap) = Value bcc (Map.mapKeys coerce maMap)

coerceMultiAsset
    :: Map (PolicyID era) (Map AssetName Integer)
    -> Map (PolicyID StandardCrypto) (Map AssetName Integer)
coerceMultiAsset = Map.mapKeys coerce

coercePoolCert :: Sophie.PoolCert era -> Sophie.PoolCert StandardCrypto
coercePoolCert pcert =
  case pcert of
    Sophie.RegPool cert -> Sophie.RegPool (coercePoolParams cert)
    Sophie.RetirePool kh e -> Sophie.RetirePool (coerce kh) e

coercePoolParams :: Sophie.PoolParams era -> Sophie.PoolParams StandardCrypto
coercePoolParams pp =
  Sophie.PoolParams
    { Sophie._poolId = coerce (Sophie._poolId pp)
    , Sophie._poolVrf = coerce (Sophie._poolVrf pp)
    , Sophie._poolPledge = Sophie._poolPledge pp
    , Sophie._poolCost  = Sophie._poolCost pp
    , Sophie._poolMargin = Sophie._poolMargin pp
    , Sophie._poolRAcnt = coerce (Sophie._poolRAcnt pp)
    , Sophie._poolOwners = Set.map coerce (Sophie._poolOwners pp)
    , Sophie._poolRelays = Sophie._poolRelays pp
    , Sophie._poolMD = Sophie._poolMD pp
    }

-- -------------------------------------------------------------------------------------------------

fromTxIn :: Maybe Word64 -> Sophie.TxIn StandardCrypto -> TxIn
fromTxIn setIndex (Sophie.TxIn (Sophie.TxId txid) index) =
  TxIn
    { txInHash = Crypto.hashToBytes $ Ledger.extractHash txid
    , txInIndex = fromIntegral index
    , txInRedeemerIndex = setIndex
    }

mkTxWithdrawal :: Maybe Word64 -> (Sophie.RewardAcnt StandardCrypto, Coin) -> TxWithdrawal
mkTxWithdrawal rIndex (ra, c) =
  TxWithdrawal
    { txwRedeemerIndex = rIndex
    , txwRewardAccount = ra
    , txwAmount = c
    }

txHashId :: SophieBasedEra era => Sophie.Tx era -> ByteString
txHashId = Crypto.hashToBytes . Ledger.extractHash . Ledger.hashAnnotated . Sophie.body

-- | 'Set.elemAt' is a partial function so we can't use it. This reverses the 'indexOf' of the
-- 'class Indexable elem container'.
elemAtSet :: forall a. Ord a => Word64 -> Set a -> Maybe a
elemAtSet n set =
    snd <$> find (\(index, _a) -> index == Just n) (Set.toList setWithIndexes)
  where
    setWithIndexes :: Set (Maybe Word64, a)
    setWithIndexes = Set.map (\a -> (strictMaybeToMaybe $ Aurum.indexOf a set, a)) set

scriptHashAcnt :: Sophie.RewardAcnt StandardCrypto -> Maybe ByteString
scriptHashAcnt rewardAddr = getCredentialScriptHash $ Ledger.getRwdCred rewardAddr

-- This mimics 'Ledger.addOnlyCwitness'
scriptHashCert :: Sophie.DCert StandardCrypto -> Maybe ByteString
scriptHashCert cert =
  case cert of
    Sophie.DCertDeleg (Sophie.DeRegKey cred) ->
      getCredentialScriptHash cred
    Sophie.DCertDeleg (Sophie.Delegate (Sophie.Delegation cred _)) ->
      getCredentialScriptHash cred
    _ -> Nothing
