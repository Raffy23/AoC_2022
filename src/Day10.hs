module Day10 (solve1, solve2, parseInput, Input, Output, Instruction (..)) where

import Data.Attoparsec.Text (Parser, char, choice, decimal, signed, endOfLine, parseOnly, sepBy, string)
import Data.Either (fromRight)
import Data.Text (pack)
import Data.List.Split (chunksOf)

data Instruction = Noop | AddX Int deriving (Show, Eq)

type Input = [Instruction]
type Output = Int

solve1 :: Input -> Output
solve1 input = foldl sumSignals 0 $ runInstr 1 1 input
  where
    sumSignals :: Int -> (Int, Int) -> Int
    sumSignals count (cycle, value)
      | cycle `mod` 40 == 20 = count + cycle * value
      | otherwise            = count

solve2 :: Input -> [String]
solve2 input = chunksOf 40 . map pixel $ (1,1) : runInstr 1 1 input

pixel :: (Int, Int) -> Char
pixel (cycle, value) = if abs (displayPosition - value) < 2 then '█' else ' '
  where
    displayPosition = (cycle-1) `mod` 40

runInstr :: Int -> Int -> [Instruction] -> [(Int, Int)]
runInstr cycle    _ []              = []
runInstr cycle regX (Noop     : is) = (cycle + 1, regX) : runInstr (cycle + 1) regX is
runInstr cycle regX ((AddX x) : is) = (cycle + 1, regX) : (cycle + 2, regX') : runInstr (cycle + 2) regX' is
  where
    regX' = regX + x

parseInput :: String -> Input
parseInput = fromRight (error "Unable to parse input") . parseOnly parseInput' . pack
  where
    parseInput' :: Parser [Instruction]
    parseInput' = instruction `sepBy` endOfLine

    instruction = choice [noop, addx]

    noop = do
      _ <- string (pack "noop")
      return Noop

    addx = do
      _ <- string (pack "addx")
      _ <- char ' '
      AddX <$> signed decimal
