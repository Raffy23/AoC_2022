module Day01 (solve1, solve2, parseInput, Input, Output) where

import Data.List (sortOn)
import Data.List.Split
import Data.Ord

type Input = [[Int]]
type Output = Int

solve1 :: Input -> Output
solve1 = maximum . map sum

solve2 :: Input -> Output
solve2 =  sum . take 3 . sortOn Down . map sum

parseInput :: String -> Input
parseInput = toIntList . groupByEmpty . lines
  where
    toIntList :: [[String]] -> [[Int]]
    toIntList = map $ map read

    groupByEmpty :: [String] -> [[String]]
    groupByEmpty = splitWhen emptyLine

    emptyLine = (==) ""
