module Day01 (solve1, solve2, parseInput, Input, Output) where

import Data.Attoparsec.Text (decimal, Parser, parseOnly, many', endOfLine)
import Data.Text (pack)
import Data.Either (fromRight)
import Data.List (sortOn)
import Data.Ord

type Input = [[Int]]
type Output = Int

solve1 :: Input -> Output
solve1 = maximum . map sum

solve2 :: Input -> Output
solve2 =  sum . take 3 . sortOn Down . map sum

parseInput :: String -> Input
parseInput = fromRight (error "Unable to parse input") . parseOnly parseInput' . pack
  where
    parseInput' :: Parser [[Int]]
    parseInput' = many' group
  
    group :: Parser [Int]
    group = do 
      lines <- many' line
      _     <- endOfLine
      return lines

    line = do
      num <- decimal
      _   <- endOfLine
      return num
