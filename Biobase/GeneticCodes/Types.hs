
-- |

module Biobase.GeneticCodes.Types where

import Control.Lens
import Data.ByteString.Char8 (ByteString)
import Data.Map.Strict (Map,fromList,fromListWith)
import Data.Text (Text)

import Biobase.Types.Codon



data TranslationElement c a = TranslationElement
  { _baseCodon    ∷ !(Codon c)
  , _isStartCodon ∷ !Bool
  , _aminoAcid    ∷ !a
  }
  deriving (Show)
makeLenses ''TranslationElement

data TranslationTable c a = TranslationTable
  { _codonToAminoAcid  ∷ !(Map (Codon c) (TranslationElement c a))
  , _aminoAcidtoCodons ∷ !(Map a [TranslationElement c a])
  , _tableID           ∷ !Int
  , _tableName         ∷ !Text
  }
  deriving (Show)
makeLenses ''TranslationTable

genTranslationTable
  ∷ (Ord c, Ord a)
  ⇒ Int
  -- ^ table identifier
  → Text
  -- ^ table hdr / table name
  → [TranslationElement c a]
  -- ^ known translation elements (should be @4^3@ but is not checked)
  → TranslationTable c a
  -- ^ finished translation table
{-# Inlinable genTranslationTable #-}
genTranslationTable i hdr xs = TranslationTable
  { _codonToAminoAcid  = fromList [ (t^.baseCodon, t) | t ← xs ]
  , _aminoAcidtoCodons = fromListWith (++) [ (t^.aminoAcid, [t]) | t ← xs ]
  , _tableID           = i
  , _tableName         = hdr
  }

