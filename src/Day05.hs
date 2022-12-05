module Day05 (solve1, solve2, parseInput, Input, Output) where

import qualified Data.Attoparsec.Text as P
import Data.List.Split
import Data.Text (pack)
import Data.Either (rights)
import Data.Array

-- import Debug.Trace

data Crate = Crate Char | EmptyCrate deriving (Show, Eq)

type StackIdx = Int
type Count = Int
data Move = Move Count StackIdx StackIdx deriving (Show)

type StackArray = Array Int [Crate]

type Input =  (StackArray, [Move])
type Output = [Char]


solve1 :: Input -> Output
solve1 (inStack, moves) = map ((\(Crate v) -> v) . head) . elems $ foldl modify inStack moves
  where
    modify :: StackArray -> Move -> StackArray
    modify stack (Move count source target) = newStack
      where
        newSource = (source, drop count $ stack ! source)
        newTarget = (target, reverse (take count $ stack ! source) ++ stack ! target)
        newStack  = stack // [newSource, newTarget]

printCrate :: StackArray -> [(Int, String)]
printCrate arr = map p2 (assocs arr)
  where
    p1 :: [Crate] -> String
    p1 = map (\(Crate v) -> v)

    p2 :: (Int, [Crate]) -> (Int, String)
    p2 (i, cs) = (i, p1 cs)

solve2 :: Input -> Output
solve2 (inStack, moves) = map ((\(Crate v) -> v) . head) . elems $ foldl modify inStack moves
   where
     modify :: StackArray -> Move -> StackArray
     modify stack (Move count source target) = newStack
       where
         newSource = (source, drop count $ stack ! source)
         newTarget = (target, take count (stack ! source) ++ stack ! target)
         newStack  = stack // [newSource, newTarget]

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
-- m // [(1, drop 3 $ m ! 1),(2, (reverse $ take 3 $ m ! 1) ++ m ! 2)]

transpose :: [[a]]->[[a]]
transpose ([]:_) = []
transpose x      = map head x : transpose (map tail x)

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