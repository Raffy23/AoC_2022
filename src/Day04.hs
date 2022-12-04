module Day04 (solve1, solve2, parseInput, Input, Output, Range (..)) where

import Data.Attoparsec.Text (decimal, Parser, char, parseOnly)
import Data.Text (pack)
import Data.Either (rights )

data Range = Range Int Int deriving Show

type Input = [(Range, Range)]
type Output = Int

solve1 :: Input -> Output
solve1 = length . filter (uncurry fullyContainsRange)

solve2 :: Input -> Output
solve2 = length . filter (uncurry overlapRange)

parseInput :: String -> Input
parseInput = rights . map (parseOnly parseInputLine . pack) . lines

parseRange :: Parser Range
parseRange = do
    start <- decimal
    _     <- char '-'
    Range start <$> decimal

parseInputLine :: Parser (Range, Range)
parseInputLine = do
    first  <- parseRange
    _      <- char ','
    second <- parseRange
    return (first, second)

fullyContainsRange :: Range -> Range -> Bool
fullyContainsRange (Range s1 e1) (Range s2 e2) = (s1 >= s2 && e1 <= e2) || (s1 <= s2 && e1 >= e2)

overlapRange :: Range -> Range -> Bool
overlapRange (Range s1 e1) (Range s2 e2) = s1 <= e2 && s2 <= e1