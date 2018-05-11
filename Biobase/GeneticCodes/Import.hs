
-- |
--
-- The tables imported with these functions are from ENA:
-- <https://www.ebi.ac.uk/ena/browse/translation-tables>

module Biobase.GeneticCodes.Import where

import Control.Monad.Except
import Data.ByteString.Char8 as BS hiding (unpack,map)
import Data.ByteString.Char8 (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO as TIO
import Data.Text (Text,unpack)
import Data.Void
import System.Exit
import Text.Megaparsec
import Text.Megaparsec.Char as MC
import Text.Megaparsec.Char.Lexer as MCL

import Biobase.GeneticCodes.Types



type TTParser = Parsec Void Text

-- | Import translation tables from a given file. In case of parse error, print
-- the error and exit with a failure.

fromFile
  ∷ (MonadIO m, MonadError String m)
  ⇒ FilePath
  → m [TranslationTable Char Char]
fromFile fp = (liftIO $ BS.readFile fp) >>= fromByteString

-- | Parse a ByteString with translation tables.

fromByteString
  ∷ (MonadError String m)
  ⇒ ByteString
  → m [TranslationTable Char Char]
fromByteString bs = case runParser (some parseTranslationTable) "" (decodeUtf8 bs) of
    Left err → throwError $ parseErrorPretty err
    Right rs → return rs

-- | Parses a single translation table.

parseTranslationTable ∷ TTParser (TranslationTable Char Char)
parseTranslationTable = do
  (i,hdr) ← parseHeader
  aas     ← parseData "amino acids"
  starts' ← parseData "start codons"
  base1   ← parseData "Base 1"
  base2   ← parseData "Base 2"
  base3   ← parseData "Base 3"
  let triplets = zipWith3 BaseTriplet base1 base2 base3
  let starts   = map (=='M') starts'
  let translations = zipWith3 TranslationElement triplets starts aas
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

