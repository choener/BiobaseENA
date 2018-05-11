
-- |

module Biobase.GeneticCodes.Types where

import Control.Lens
import Data.ByteString.Char8 (ByteString)
import Data.Map.Strict (Map,fromList,fromListWith)
import Data.Text (Text)



data BaseTriplet c = BaseTriplet !c !c !c
  deriving (Show,Eq,Ord)

-- | Handle triplets like a list. In particular
-- @over tripletChars show (BaseTriplet 1 2 3) == BaseTriplet "1" "2" "3"@.
-- Operations like these are useful for transforming the translation tables.

tripletChars ∷ Lens (BaseTriplet c) (BaseTriplet c') [c] [c']
{-# Inline tripletChars #-}
tripletChars = lens from to
  where from (BaseTriplet a b c) = [a,b,c]
        to (BaseTriplet a b c) [d,e,f] = BaseTriplet d e f

data TranslationElement c a = TranslationElement
  { _baseTriplet  ∷ !(BaseTriplet c)
  , _isStartCodon ∷ !Bool
  , _aminoAcid    ∷ !a
  }
  deriving (Show)
makeLenses ''TranslationElement

data TranslationTable c a = TranslationTable
  { _tripletToAminoAcid   ∷ !(Map (BaseTriplet c) (TranslationElement c a))
  , _aminoAcidtoTriplets  ∷ !(Map a [TranslationElement c a])
  , _tableID              ∷ !Int
  , _tableName            ∷ !Text
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
  { _tripletToAminoAcid  = fromList [ (t^.baseTriplet, t) | t ← xs ]
  , _aminoAcidtoTriplets = fromListWith (++) [ (t^.aminoAcid, [t]) | t ← xs ]
  , _tableID             = i
  , _tableName           = hdr
  }

