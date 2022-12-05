module Day05 (solve1, solve2, parseInput, Input, Output, Crate(..), Move(..)) where

import qualified Data.Attoparsec.Text as P
import Data.List.Split
import Data.Text (pack)
import Data.Either (rights)
import Data.Array

import Common (transpose)

-- import Debug.Trace

data Crate = Crate Char | EmptyCrate deriving (Show, Eq)

type StackIdx = Int
type Count = Int
data Move = Move Count StackIdx StackIdx deriving (Show)

type StackArray = Array Int [Crate]

type Input =  (StackArray, [Move])
type Output = [Char]


solve1 :: Input -> Output
solve1 = moveCrates reverse

solve2 :: Input -> Output
solve2 = moveCrates id

moveCrates :: ([Crate] -> [Crate]) -> Input -> Output
moveCrates order (inStack, moves) = printTopCrates . elems $ foldl modify inStack moves
  where
    printTopCrates :: [[Crate]] -> [Char]
    printTopCrates = map $ (\(Crate v) -> v) . head

    modify :: StackArray -> Move -> StackArray
    modify stack (Move count source target) = stack // [newSource, newTarget]
       where
         newSource = (source, drop count $ stack ! source)
         newTarget = (target, order (take count $ stack ! source) ++ stack ! target)

parseInput :: String -> Input
parseInput = parse . groupByEmpty . lines
  where
    parse :: [[String]] -> (StackArray, [Move])
    parse [cs, is] = (toArray . transpose $ creates cs, moves is)
      where
        creates :: [String] -> [[Crate]]
        creates = rights . map (P.parseOnly parseCrateRow . pack)

        moves :: [String] -> [Move]
        moves = rights . map (P.parseOnly parseMove . pack)

        toArray :: [[Crate]] -> StackArray
        toArray stacks = array (1, length stacks) $ zip [1..] $ map (dropWhile (EmptyCrate ==)) stacks

    groupByEmpty :: [String] -> [[String]]
    groupByEmpty = splitWhen emptyLine

    emptyLine = (==) ""

parseEmptyCrate :: P.Parser Crate
parseEmptyCrate = do
  _ <- P.string $ pack "   "
  return EmptyCrate

parseCrate :: P.Parser Crate
parseCrate = do
  _ <- P.char '['
  c <- P.letter
  _ <- P.char ']'
  return $ Crate c

parseCrateRow :: P.Parser [Crate]
parseCrateRow = do
  P.many1 $ do
     c <- P.choice [parseCrate, parseEmptyCrate]
     _ <- P.option ' ' $ P.char ' '
     return c

parseMove :: P.Parser Move
parseMove = do
  _      <- P.string $ pack "move "
  count  <- P.decimal
  _      <- P.string $ pack " from "
  source <- P.decimal
  _      <- P.string $ pack " to "
  target <- P.decimal
  return $ Move count source target