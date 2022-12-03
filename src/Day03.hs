module Day03 (solve1, solve2, parseInput, Input, Output, splitItem) where

import Data.Char (ord, isAsciiLower, isAsciiUpper)
import Data.List.Split (chunksOf)

import qualified Data.Set as S

type Input = [String]
type Output = Int

solve1 :: Input -> Output
solve1 input = sum $ map (itemPriority . intersect . splitItem) input

solve2 :: Input -> Output
solve2 input = sum $ map (itemPriority . intersect) (chunksOf 3 input)

parseInput :: String -> Input
parseInput = lines

splitItem :: String -> [String]
splitItem str = (\(x,y) -> [x,y]) (splitAt middle str)
  where
    middle :: Int
    middle = div (length str) 2

priority :: Char -> Int
priority c
  | isAsciiLower c = ord c - 96
  | isAsciiUpper c = ord c - 38
  | otherwise      = error "Unkown character"

itemPriority :: String -> Int
itemPriority = sum . map priority

intersect :: [String] -> String
intersect = S.elems . foldl1 S.intersection . map S.fromList
