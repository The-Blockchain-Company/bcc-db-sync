{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Bcc.DbSync.Era.Sophie.Generic.Block
  ( Block (..)
  , BlockEra (..)
  , fromSophieBlock
  , fromAllegraBlock
  , fromJenBlock
  , fromAurumBlock

  , slotLeaderHash
  ) where

import qualified Bcc.Api.Sophie as Api

import qualified Bcc.Crypto.Hash as Crypto
import qualified Bcc.Crypto.KES.Class as KES

import           Bcc.Crypto.VRF.Optimum (OptimumVRF)

import           Bcc.DbSync.Era.Sophie.Generic.Tx
import           Bcc.DbSync.Era.Sophie.Generic.Util

import           Bcc.Ledger.Aurum ()
import           Bcc.Ledger.Core (Witnesses)
import qualified Bcc.Ledger.Core as Ledger
import           Bcc.Ledger.Crypto (VRF)
import           Bcc.Ledger.Era (Crypto, SupportsSegWit (..))
import qualified Bcc.Ledger.Era as Ledger
import           Bcc.Ledger.SafeHash (SafeToHash)

import           Bcc.Prelude

import           Bcc.Slotting.Slot (SlotNo (..))

import           Shardagnostic.Consensus.Bcc.Block (StandardAllegra, StandardAurum, StandardJen,
                   StandardSophie)
import           Shardagnostic.Consensus.Sophie.Ledger.Block (SophieBasedEra, SophieBlock)
import qualified Shardagnostic.Consensus.Sophie.Ledger.Block as Consensus

import           Shardagnostic.Network.Block (BlockNo (..))

import qualified Sophie.Spec.Ledger.BlockChain as Sophie
import qualified Sophie.Spec.Ledger.OCert as Sophie
import qualified Sophie.Spec.Ledger.PParams as Sophie
import qualified Sophie.Spec.Ledger.Tx as Sophie


data BlockEra
  = Sophie
  | Evie
  | Jen
  | Aurum
  deriving (Eq, Show)

data Block = Block
  { blkEra :: !BlockEra
  , blkHash :: !ByteString
  , blkPreviousHash :: !ByteString
  , blkCreatorPoolHash :: !ByteString
  , blkSlotLeader :: !ByteString
  , blkSlotNo :: !SlotNo
  , blkBlockNo :: !BlockNo
  , blkSize :: !Word64
  , blkProto :: !Sophie.ProtVer
  , blkVrfKey :: !Text
  , blkOpCert :: !ByteString
  , blkOpCertCounter :: !Word64
  , blkTxs :: ![Tx]
  }


fromAllegraBlock :: SophieBlock StandardAllegra -> Block
fromAllegraBlock blk =
  Block
    { blkEra = Evie
    , blkHash = blockHash blk
    , blkPreviousHash = blockPrevHash blk
    , blkCreatorPoolHash = creatorPoolHash blk
    , blkSlotLeader = slotLeaderHash blk
    , blkSlotNo = slotNumber blk
    , blkBlockNo = blockNumber blk
    , blkSize = blockSize blk
    , blkProto = blockProtoVersion blk
    , blkVrfKey = blockVrfKeyView blk
    , blkOpCert = blockOpCert blk
    , blkOpCertCounter = blockOpCertCounter blk
    , blkTxs = map fromAllegraTx (blockTxs blk)
    }

fromSophieBlock :: SophieBlock StandardSophie -> Block
fromSophieBlock blk =
  Block
    { blkEra = Sophie
    , blkHash = blockHash blk
    , blkPreviousHash = blockPrevHash blk
    , blkCreatorPoolHash = creatorPoolHash blk
    , blkSlotLeader = slotLeaderHash blk
    , blkSlotNo = slotNumber blk
    , blkBlockNo = blockNumber blk
    , blkSize = blockSize blk
    , blkProto = blockProtoVersion blk
    , blkVrfKey = blockVrfKeyView blk
    , blkOpCert = blockOpCert blk
    , blkOpCertCounter = blockOpCertCounter blk
    , blkTxs = map fromSophieTx (blockTxs blk)
    }

fromJenBlock :: SophieBlock StandardJen -> Block
fromJenBlock blk =
  Block
    { blkEra = Jen
    , blkHash = blockHash blk
    , blkPreviousHash = blockPrevHash blk
    , blkCreatorPoolHash = creatorPoolHash blk
    , blkSlotLeader = slotLeaderHash blk
    , blkSlotNo = slotNumber blk
    , blkBlockNo = blockNumber blk
    , blkSize = blockSize blk
    , blkProto = blockProtoVersion blk
    , blkVrfKey = blockVrfKeyView blk
    , blkOpCert = blockOpCert blk
    , blkOpCertCounter = blockOpCertCounter blk
    , blkTxs = map fromJenTx (blockTxs blk)
    }

fromAurumBlock :: Ledger.PParams StandardAurum -> SophieBlock StandardAurum -> Block
fromAurumBlock pp blk =
  Block
    { blkEra = Aurum
    , blkHash = blockHash blk
    , blkPreviousHash = blockPrevHash blk
    , blkCreatorPoolHash = creatorPoolHash blk
    , blkSlotLeader = slotLeaderHash blk
    , blkSlotNo = slotNumber blk
    , blkBlockNo = blockNumber blk
    , blkSize = blockSize blk
    , blkProto = blockProtoVersion blk
    , blkVrfKey = blockVrfKeyView blk
    , blkOpCert = blockOpCert blk
    , blkOpCertCounter = blockOpCertCounter blk
    , blkTxs = map (fromAurumTx pp) (aurumBlockTxs blk)
    }

-- -------------------------------------------------------------------------------------------------

aurumBlockTxs :: SophieBlock StandardAurum -> [(Word64, Ledger.Tx StandardAurum)]
aurumBlockTxs = zip [0 ..] . toList . fromTxSeq @StandardAurum . Sophie.bbody . Consensus.sophieBlockRaw

blockBody :: SophieBasedEra era => SophieBlock era -> Sophie.BHBody (Crypto era)
blockBody = Sophie.bhbody . Sophie.bheader . Consensus.sophieBlockRaw

blockHash :: SophieBlock era -> ByteString
blockHash =
  Crypto.hashToBytes . Sophie.unHashHeader
    . Consensus.unSophieHash . Consensus.sophieBlockHeaderHash

blockNumber :: SophieBasedEra era => SophieBlock era -> BlockNo
blockNumber = Sophie.bheaderBlockNo . blockBody

blockPrevHash :: SophieBasedEra era => SophieBlock era -> ByteString
blockPrevHash blk =
  case Sophie.bheaderPrev (Sophie.bhbody . Sophie.bheader $ Consensus.sophieBlockRaw blk) of
    Sophie.GenesisHash -> "Bcc.DbSync.Era.Sophie.Generic.Block.blockPrevHash"
    Sophie.BlockHash h -> Crypto.hashToBytes (Sophie.unHashHeader h)

blockOpCert :: SophieBasedEra era => SophieBlock era -> ByteString
blockOpCert = KES.rawSerialiseVerKeyKES . Sophie.ocertVkHot . Sophie.bheaderOCert . blockBody

blockOpCertCounter :: SophieBasedEra era => SophieBlock era -> Word64
blockOpCertCounter = Sophie.ocertN . Sophie.bheaderOCert . blockBody

blockProtoVersion :: SophieBasedEra era => SophieBlock era -> Sophie.ProtVer
blockProtoVersion = Sophie.bprotver . blockBody

blockSize :: SophieBasedEra era => SophieBlock era -> Word64
blockSize = fromIntegral . Sophie.bBodySize . Sophie.bbody . Consensus.sophieBlockRaw

blockTxs
    :: ( SophieBasedEra era
        , Ledger.TxSeq era ~ Sophie.TxSeq era
        , SafeToHash (Witnesses era)
        )
    => SophieBlock era -> [(Word64, Sophie.Tx era)]
blockTxs = zip [0 ..] . unTxSeq . Sophie.bbody . Consensus.sophieBlockRaw

blockVrfKeyView :: (SophieBasedEra era, VRF (Crypto era) ~ OptimumVRF) => SophieBlock era -> Text
blockVrfKeyView = Api.serialiseToBech32 . Api.VrfVerificationKey . Sophie.bheaderVrfVk . blockBody

creatorPoolHash :: SophieBasedEra era => SophieBlock era -> ByteString
creatorPoolHash = unKeyHashRaw . Sophie.issuerIDfromBHBody . blockBody

slotLeaderHash :: SophieBasedEra era => SophieBlock era -> ByteString
slotLeaderHash = unKeyHashRaw . Sophie.issuerIDfromBHBody . blockBody

slotNumber :: SophieBasedEra era => SophieBlock era -> SlotNo
slotNumber = Sophie.bheaderSlotNo . blockBody

unTxSeq
    :: (SophieBasedEra era, SafeToHash (Witnesses era))
    => Sophie.TxSeq era -> [Sophie.Tx era]
unTxSeq (Sophie.TxSeq txSeq) = toList txSeq
