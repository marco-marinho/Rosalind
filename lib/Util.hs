module Util (Fasta, parseInput, header, sequenceData) where

import Text.Parsec
import Text.Parsec.String (Parser)

data Fasta = Fasta {header :: String, sequenceData :: String} deriving (Show, Eq)

parseHeader :: Parser String
parseHeader = char '>' *> manyTill anyChar newline

parseDNA :: Parser String
parseDNA = concat <$> many1 (many1 (oneOf "ACGT") <* spaces)

parseFasta :: Parser Fasta
parseFasta = do
  header <- parseHeader
  Fasta header <$> parseDNA

fastaFile :: Parser [Fasta]
fastaFile = many1 parseFasta <* eof

parseInput :: String -> [Fasta]
parseInput input = case parse fastaFile "" input of
  Left err -> error $ show err
  Right fastas -> fastas