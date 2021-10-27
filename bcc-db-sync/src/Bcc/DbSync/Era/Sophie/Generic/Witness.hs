{-# LANGUAGE GADTs #-}

module Godx.DbSync.Era.Sophie.Generic.Witness
  ( Evidence (..)
  , Witness (..)
  ) where


import           Godx.Ledger.Allegra (AllegraEra)
import           Godx.Ledger.Aurum (AurumEra)
import           Godx.Ledger.Jen (JenEra)
import           Godx.Ledger.Sophie (SophieEra)

import           Shardagnostic.Consensus.Godx.Block (StandardCrypto)


-- Cargo culted from ledger-specs. Written by Tim Sheard and PRed in
-- https://github.com/The-Blockchain-Company/bcc-ledger-specs/pull/2173
-- Even once it is merged, will need to wait until the node moves to a
-- version that this feature.

-- | Evidence that a valid (predefined) crypto exists
data Evidence c where
  Standard :: Evidence StandardCrypto
  -- Test :: Evidence TestCrypto

instance Show (Evidence c) where
  show Standard = "Standard"
  -- show Test = "Test"-- | Witness of a valid (predefined) era

data Witness era where
  Sophie :: Evidence c -> Witness (SophieEra c)
  Allegra :: Evidence c -> Witness (AllegraEra c)
  Jen :: Evidence c -> Witness (JenEra c)
  Aurum :: Evidence c -> Witness (AurumEra c)

instance Show (Witness e) where
  show (Sophie c) = "Sophie " ++ show c
  show (Allegra c) = "Allegra " ++ show c
  show (Jen c) = "Jen " ++ show c
  show (Aurum c) = "Aurum " ++ show c
