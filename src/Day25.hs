{-# LANGUAGE LambdaCase #-}

module Day25 (solve1, parseInput, Input, Output) where

import Data.Attoparsec.Text (choice, parseOnly, char, sepBy, many1, endOfLine)
import Data.Text (pack)
import Data.Either (fromRight)
import Data.List (unfoldr)

type Input = [String]
type Output = String

solve1 :: Input -> Output
solve1 = showSnafu . sum . map parseSnafu

parseInput :: String -> Input
parseInput = fromRight (error "Unable to parse input") . parseOnly (snafuSymbols `sepBy` endOfLine) . pack
  where
    snafuSymbols = many1 $ choice [char '1', char '2', char '0', char '-', char '=']

parseSnafu :: String -> Int
parseSnafu = snd . foldr (\sym (pos, n) -> (pos + 1, n + value sym * 5 ^ pos)) (0 :: Int, 0)
  where
    value :: Char -> Int
    value '=' = -2
    value '-' = -1
    value '0' =  0
    value '1' =  1
    value '2' =  2

showSnafu :: Int -> String
showSnafu = reverse . unfoldr (\case
    0 -> Nothing
    n -> value $ divMod n 5
  )
  where
    value :: (Int, Int) -> Maybe (Char, Int)
    value (n', 0) = Just ('0', n' + 0)
    value (n', 1) = Just ('1', n' + 0)
    value (n', 2) = Just ('2', n' + 0)
    value (n', 3) = Just ('=', n' + 1)
    value (n', 4) = Just ('-', n' + 1)