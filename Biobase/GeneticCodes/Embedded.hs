
module Biobase.GeneticCodes.Embedded where

import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.FileEmbed

import Biobase.GeneticCodes.Import
import Biobase.GeneticCodes.Types

geneticCodesFile ∷ ByteString
geneticCodesFile = $(embedFile "sources/translation-tables")

geneticCodes ∷ [TranslationTable Char Char]
geneticCodes = either error id . runExcept $ fromByteString geneticCodesFile

