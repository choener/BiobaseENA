
module Biobase.GeneticCodes
  ( module Biobase.GeneticCodes
  , module Biobase.GeneticCodes.Types
  , fromByteString
  , fromFile
  , geneticCodes
  , translate
  ) where

import Control.Lens
import Control.Monad.Except
import Data.List (find)
import Data.Text (pack)
import Data.Text (Text,isInfixOf,unpack)
import System.Directory (doesFileExist)
import Text.Printf
import System.Exit (exitSuccess)

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

-- | If the given filepath exists, then we try to load the genetic table from
-- the file. This will fail if there is not exactly one genetic table there. If
-- @fp@ is not a file, we try parsing @fp@ as a numeric ID and look for that
-- table. Finally we try finding an infix with that name.
--
-- This is all slightly "unsafe" but captures the most common scenario where we
-- either load such a table from file or need selection of the correct one.
--
-- If the given filepath is @"list"@, then a list of table id's and table names
-- is returned.

fromFileOrCached
  ∷ (MonadIO m, MonadError String m)
  ⇒ FilePath
  → m (TranslationTable Char Char)
fromFileOrCached fp = do
  dfe ← liftIO $ doesFileExist fp
  if | fp == "list" → do
          mapM_ (liftIO . uncurry (printf "%3d %s\n")) [ (t^.tableID,t^.tableName) | t ← geneticCodes ]
          liftIO exitSuccess
     | dfe → fromFile fp >>= \case
         [x] → return x
         xs  → throwError $ fp ++ " should contain exactly one translation table!"
     | [(k,"")] ← reads fp → codeByTableID geneticCodes k
     | otherwise → codeByTableNameInfix geneticCodes $ pack fp

