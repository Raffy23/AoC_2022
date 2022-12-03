module Day03 (solve1, solve2, parseInput, Input, Output, splitItem) where

import Data.Char (ord, isAsciiLower, isAsciiUpper)
import Data.List.Split (chunksOf)

import qualified Data.Set as S

type Input = [String]
type Output = Int

solve1 :: Input -> Output
solve1 input = sum $ map (itemPriority . intersect . toSetPair . splitItem) input

solve2 :: Input -> Output
solve2 input = sum $ map (itemPriority . intersect' . map S.fromList) (chunksOf 3 input)

parseInput :: String -> Input
parseInput = lines

splitItem :: String -> (String, String)
splitItem str = splitAt (middle str) str
  where
    middle :: String -> Int
    middle str = div (length str) 2

priority :: Char -> Int
priority c
  | isAsciiLower c = ord c - 96
  | isAsciiUpper c = ord c - 38
  | otherwise      = error "Unkown character"

itemPriority :: String -> Int
itemPriority = sum . map priority

toSetPair :: (String, String) -> (S.Set Char, S.Set Char)
toSetPair (a, b) = (S.fromList a, S.fromList b)

intersect :: (S.Set Char, S.Set Char) -> String
intersect (a, b) = S.elems $ S.intersection a b

intersect' :: [S.Set Char] -> String
intersect' = S.elems . foldl1 S.intersection