
module Biobase.GeneticCodes
  ( module Biobase.GeneticCodes
  , module Biobase.GeneticCodes.Types
  , fromByteString
  , fromFile
  , geneticCodes
  ) where

import Control.Monad.Except
import Data.List (find)
import Text.Printf
import Control.Lens
import Data.Text (Text,isInfixOf,unpack)

import Biobase.GeneticCodes.Embedded
import Biobase.GeneticCodes.Import
import Biobase.GeneticCodes.Types



codeByTableID ∷ (Monad m) ⇒ [TranslationTable c a] → Int → ExceptT String m (TranslationTable c a)
codeByTableID ts i
  = maybe (throwError $ printf "No TranslationTable with ID %d found!" i) return
  $ find (\t → t^.tableID == i) ts

codeByTableNameInfix ∷ (Monad m) ⇒ [TranslationTable c a] → Text → ExceptT String m (TranslationTable c a)
codeByTableNameInfix ts n
  = maybe (throwError $ printf "No TranslationTable with Name infix %s found!" $ unpack n) return
  $ find (\t → n `isInfixOf` (t^.tableName)) ts

