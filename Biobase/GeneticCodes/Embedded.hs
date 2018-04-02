
module Biobase.GeneticCodes.Embedded where

import Data.ByteString (ByteString)
import Data.FileEmbed

import Biobase.GeneticCodes.Import
import Biobase.GeneticCodes.Types

geneticCodesFile ∷ ByteString
geneticCodesFile = $(embedFile "sources/translation-tables")

geneticCodes ∷ [TranslationTable]
geneticCodes = fromByteString geneticCodesFile

