
-- | Abstract translation functionality. Given a genetic code translation table
-- (as provided within this module), provide translation from the nucleotide to
-- the amino acid alphabet.
--
-- Limited "backtranslation" capabilities are provided. Since this process is
-- lossy, it should only be used in very specific circumstances.
--
-- TODO Translation from @BioSequence RNA is missing.

module Biobase.GeneticCodes.Translation where

import           Control.Lens
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as BS

import           Biobase.Types.BioSequence

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

-- | Strings of characters are normally very inconvenient but useful in
-- backtracking cases. Fully assumes that the alphabet is DNA. Ignores
-- non-triplets at the end.

instance Translation String where
  type TargetType String = String
  type TripletType String = Char
  type AAType String = Char
  translate tbl =
    let go xs | [x,y,z] ← hd = translate tbl (BaseTriplet x y z) : go tl
              | otherwise = []
              where (hd,tl) = splitAt 3 xs
    in  go

-- | Translation of @BioSequence DNA@. The translation tables assume DNA
-- triplets anyway. Biologically there should be a transcription step in
-- between. Ignores non-triplets at the end.

instance Translation (BioSequence DNA) where
  type TargetType (BioSequence DNA) = BioSequence AA
  type TripletType (BioSequence DNA) = Char
  type AAType (BioSequence DNA) = Char
  translate tbl (BioSequence xs) =
    let go k = Just (translate tbl $ BaseTriplet (BS.index xs k) (BS.index xs (k+1)) (BS.index xs (k+2)) ,k+3)
    in  BioSequence . fst $ BS.unfoldrN (BS.length xs `div` 3) go 0

