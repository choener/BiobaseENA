
-- | Abstract translation functionality. Given a genetic code translation table
-- (as provided within this module), provide translation from the nucleotide to
-- the amino acid alphabet.
--
-- Limited "backtranslation" capabilities are provided. Since this process is
-- lossy, it should only be used in very specific circumstances.

module Biobase.GeneticCodes.Translation where

import           Control.Lens
import qualified Data.Map.Strict as M

import           Biobase.GeneticCodes.Types



-- |

class Translation t where
  -- | Defines the target type for a given translation input.
  type TargetType t ∷ *
  -- | Type of the nucleotide characters.
  type TripletType t ∷ *
  -- | Type of the amino acid characters.
  type AAType t ∷ *
  -- | Translate from a given type of sequence @t@ into the target type.
  translate ∷ TranslationTable (TripletType t) (AAType t) → t → TargetType t

-- | Very simple translation of individual base triplets.

instance Translation (BaseTriplet Char) where
  type TargetType  (BaseTriplet Char) = Char
  type TripletType (BaseTriplet Char) = Char
  type AAType      (BaseTriplet Char) = Char
  translate tbl t = maybe 'X' _aminoAcid $ M.lookup t (tbl^.tripletToAminoAcid)
  {-# Inline translate #-}

