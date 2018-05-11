
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
  -- | Translate from a given type of sequence @t@ into the target type.
  translate ∷ TranslationTable → t → TargetType t

-- | Very simple translation of individual base triplets.

instance Translation BaseTriplet where
  type TargetType BaseTriplet = Char
  translate tbl t = M.findWithDefault (error "default AA element? Or is this total?") t (tbl^.tripletToAminoAcid) ^. aminoAcid
  {-# Inline translate #-}

