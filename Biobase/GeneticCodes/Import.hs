
module Biobase.GeneticCodes.Import where

import Data.ByteString.Char8 (ByteString)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char as MC
import Text.Megaparsec.Char.Lexer as MCL
import Data.Text (Text,unpack)
import Data.Text.IO as TIO
import System.Exit

import Biobase.GeneticCodes.Types

type TTParser = Parsec Void Text

-- | Import translation tables from a given file. In case of parse error, print
-- the error and exit with a failure.

fromFile ∷ FilePath → IO [TranslationTable]
fromFile fp = do
  res ← runParser (some parseTranslationTable) fp <$> TIO.readFile fp
  case res of
    Left err → Prelude.putStrLn (parseErrorPretty err) >> exitFailure
    Right rs → return rs

-- | Parses a single translation table.

parseTranslationTable ∷ TTParser TranslationTable
parseTranslationTable = do
  (i,hdr) ← parseHeader
  aas     ← parseData "amino acids"
  starts' ← parseData "start codons"
  base1   ← parseData "Base 1"
  base2   ← parseData "Base 2"
  base3   ← parseData "Base 3"
  let triplets = zipWith3 BaseTriplet base1 base2 base3
  let starts   = map (=='M') starts'
  let translations = zipWith3 Translation triplets starts aas
  return $ genTranslationTable i hdr translations

-- | Parse the header, returning the Identifier and the name of the table.

parseHeader ∷ TTParser (Int,Text)
parseHeader
  = (,) <$> (fromIntegral <$> lexeme MC.space decimal)
  <* char ':' <* MC.space
  <*> takeWhileP Nothing (/= '\n')
  <* MC.space

parseData ∷ Text → TTParser String
parseData t
  = string' t <* MC.space
  *> (unpack <$> takeP Nothing 64)
  <* MC.space
