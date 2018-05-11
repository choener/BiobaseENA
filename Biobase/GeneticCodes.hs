
module Biobase.GeneticCodes
  ( module Biobase.GeneticCodes
  , module Biobase.GeneticCodes.Types
  , fromByteString
  , fromFile
  , geneticCodes
  , translate
  ) where

import Control.Monad.Except
import Data.List (find)
import Text.Printf
import Control.Lens
import Data.Text (Text,isInfixOf,unpack)
import System.Directory (doesFileExist)

import Biobase.GeneticCodes.Embedded
import Biobase.GeneticCodes.Import
import Biobase.GeneticCodes.Translation
import Biobase.GeneticCodes.Types



codeByTableID
  ∷ (MonadError String m)
  ⇒ [TranslationTable c a]
  → Int
  → m (TranslationTable c a)
codeByTableID ts i
  = maybe (throwError $ printf "No TranslationTable with ID %d found!" i) return
  $ find (\t → t^.tableID == i) ts

codeByTableNameInfix
  ∷ (MonadError String m)
  ⇒ [TranslationTable c a]
  → Text
  → m (TranslationTable c a)
codeByTableNameInfix ts n
  = maybe (throwError $ printf "No TranslationTable with Name infix %s found!" $ unpack n) return
  $ find (\t → n `isInfixOf` (t^.tableName)) ts

fromFileOrCached
  ∷ (MonadIO m, MonadError String m)
  ⇒ FilePath
  → m [TranslationTable Char Char]
fromFileOrCached fp = do
  dfe ← liftIO $ doesFileExist fp
  if dfe
    then fromFile fp
    else pure geneticCodes

