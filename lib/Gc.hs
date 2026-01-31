module Gc (solveGc) where

import Data.Foldable (maximumBy)
import Text.Parsec
import Text.Parsec.String (Parser)

data Fasta = Fasta {header :: String, sequenceData :: String} deriving (Show)

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

gcCount :: String -> Int -> Int
gcCount [] gcs = gcs
gcCount (x : xs) gcs = case x of
  'G' -> gcCount xs (gcs + 1)
  'C' -> gcCount xs (gcs + 1)
  _ -> gcCount xs gcs

gcContent :: Fasta -> Double
gcContent fasta = gcRatio
  where
    seqData = sequenceData fasta
    gcNum = gcCount seqData 0
    totalNum = length seqData
    gcRatio = (fromIntegral gcNum / fromIntegral totalNum) * 100.0

highestGcContent :: [Fasta] -> (String, Double)
highestGcContent fastas = bestFasta
  where
    gcContents = [(header f, gcContent f) | f <- fastas]
    bestFasta = maximumBy (\(_, g1) (_, g2) -> compare g1 g2) gcContents

solveGc :: String -> String
solveGc input = show $ highestGcContent (parseInput input)