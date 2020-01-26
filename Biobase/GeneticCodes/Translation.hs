
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
import           Biobase.Types.Codon

import           Biobase.GeneticCodes.Types



-- |

class Translation t where
  -- | Defines the target type for a given translation input.
  type TargetType t :: *
  -- | Type of the nucleotide characters.
  type CodonType t :: *
  -- | Type of the amino acid characters.
  type AAType t :: *
  -- | Translate from a given type of sequence @t@ into the target type.
  translate :: TranslationTable (CodonType t) (AAType t) -> t -> TargetType t
  -- | This function works just like @translate@ but with an important difference of creating a
  -- target sequence that contains all possible frames. The index @mod 3@ yields the current frame.
  translateAllFrames :: TranslationTable (CodonType t) (AAType t) -> t -> TargetType t

-- | Very simple translation of individual base triplets.

instance Translation (Codon Char) where
  type TargetType  (Codon Char) = Char
  type CodonType (Codon Char) = Char
  type AAType      (Codon Char) = Char
  translate tbl t = maybe 'X' _aminoAcid $ M.lookup t (tbl^.codonToAminoAcid)
  {-# Inline translate #-}
  translateAllFrames = translate
  {-# Inline translateAllFrames #-}

-- | Strings of characters are normally very inconvenient but useful in
-- backtracking cases. Fully assumes that the alphabet is DNA. Ignores
-- non-triplets at the end.

instance Translation String where
  type TargetType String = String
  type CodonType String = Char
  type AAType String = Char
  translate tbl =
    let go xs | [x,y,z] ← hd = translate tbl (Codon x y z) : go tl
              | otherwise = []
              where (hd,tl) = splitAt 3 xs
    in  go
  {-# Inlinable translate #-}
  translateAllFrames tbl = go []
    where go _     []     = []
          -- first two AA are unknown @?@ since the codon has not been established at this point.
          go []    (x:xs) = '?' : go [x] xs
          go [p]   (x:xs) = '?' : go [p,x] xs
          go [p,q] (x:xs) = translate tbl (Codon p q x) : go [q,x] xs
  {-# Inlinable translateAllFrames #-}

-- | Translation of @BioSequence DNA@. The translation tables assume DNA
-- triplets anyway. Biologically there should be a transcription step in
-- between. Ignores non-triplets at the end.

instance Translation (BioSequence DNA) where
  type TargetType (BioSequence DNA) = BioSequence AA
  type CodonType (BioSequence DNA) = Char
  type AAType (BioSequence DNA) = Char
  translate tbl (BioSequence xs) =
    let go k = Just (translate tbl $ Codon (BS.index xs k) (BS.index xs (k+1)) (BS.index xs (k+2)) ,k+3)
    in  BioSequence . fst $ BS.unfoldrN (BS.length xs `div` 3) go 0
  {-# Inlinable translate #-}
  translateAllFrames tbl (BioSequence xs) = BioSequence . fst $ BS.unfoldrN (BS.length xs) go 0
    where go 0 = Just ('?', 1)
          go 1 = Just ('?', 2)
          go k = Just (translate tbl $ Codon (BS.index xs (k-2)) (BS.index xs (k-1)) (BS.index xs k), k+1)
  {-# Inlinable translateAllFrames #-}

