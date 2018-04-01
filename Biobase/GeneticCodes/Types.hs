
module Biobase.GeneticCodes.Types where

import Control.Lens
import Data.ByteString.Char8 (ByteString)
import Data.Map.Strict (Map,fromList,fromListWith)
import Data.Text (Text)



data BaseTriplet = BaseTriplet !Char !Char !Char
  deriving (Show,Eq,Ord)

data Translation = Translation
  { _baseTriplet  ∷ !BaseTriplet
  , _isStartCodon ∷ !Bool
  , _aminoAcid    ∷ !Char
  }
  deriving (Show)
makeLenses ''Translation

data TranslationTable = TranslationTable
  { _tripletToAminoAcid   ∷ !(Map BaseTriplet Translation)
  , _aminoAcidtoTriplets  ∷ !(Map Char [Translation])
  , _tableID              ∷ !Int
  , _tableName            ∷ !Text
  }
  deriving (Show)
makeLenses ''TranslationTable

genTranslationTable ∷ Int → Text → [Translation] → TranslationTable
genTranslationTable i hdr xs = TranslationTable
  { _tripletToAminoAcid  = fromList [ (t^.baseTriplet, t) | t ← xs ]
  , _aminoAcidtoTriplets = fromListWith (++) [ (t^.aminoAcid, [t]) | t ← xs ]
  , _tableID             = i
  , _tableName           = hdr
  }

