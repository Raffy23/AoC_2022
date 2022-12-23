module Day23 (solve1, solve2, parseInput, Input, Output) where

import Data.Attoparsec.Text (decimal, Parser, parseOnly, many', endOfLine, char, choice, sepBy)
import Data.Text (pack)
import Data.Either (fromRight)
import Data.List (find)
import Data.Ord

import qualified Data.Set as SE
import qualified Data.Map as M
import qualified Data.Sequence as S

import Debug.Trace

type Point = (Int, Int)

data Direction = North | South | West | East | None deriving (Show, Eq)
newtype Elf = Elf (S.Seq Direction) deriving (Show, Eq)

type Input = M.Map (Int, Int) Elf
type Output = Int

solve1 :: Input -> Output
solve1 input = length [ (r,c) | r <- [minR..maxR], c <- [minC..maxC], M.notMember (r,c) input' ]
  where
    input' = Day23.round' input 10
    minR = SE.findMin $ SE.map fst $ M.keysSet input'
    minC = SE.findMin $ SE.map snd $ M.keysSet input'
    maxR = SE.findMax $ SE.map fst $ M.keysSet input'
    maxC = SE.findMax $ SE.map snd $ M.keysSet input'

solve2 :: Input -> Output
solve2 input = round'' input 1

round' :: Input -> Int -> Input
round' input 0 = input
round' input count = Day23.round' (Day23.round input) (count - 1)

round'' :: Input -> Int -> Int
round'' input count
  | M.keysSet input == M.keysSet input' = count
  | otherwise                           = round'' input' (count + 1)
  where
    input' = Day23.round input

round :: Input -> Input
round input = foldl tryMove M.empty proposed
  where
    tryMove m (old, elf, new)
      | points' M.! new == 1 = M.insert new elf m
      | otherwise            = M.insert old elf m
    proposed = map (uncurry (proposeMove input)) $ M.assocs input
    points'  = foldl (\m (_, _, point) -> M.alter update point m) M.empty proposed
    update m = case m of
      Just c  -> Just $ c + 1
      Nothing -> Just 1

proposeMove :: Input -> Point -> Elf -> (Point, Elf, Point)
proposeMove input point elf@(Elf dirs@(d0 S.:<| ds))
  | shouldMove input point = (point, elf', move point targetDir)
  | otherwise              = (point, elf', point)
  where
    (elf', targetDir) = (Elf (ds S.|> d0), propose dirs)

    propose :: S.Seq Direction -> Direction
    propose S.Empty = None
    propose (m S.:<| ms)
      | isFree input point m = m
      | otherwise            = propose ms

shouldMove :: Input -> Point -> Bool
shouldMove input (r, c) = any (`M.member` input)
  [ (r-1, c-1), (r-1, c), (r-1, c+1)
  , (r  , c-1),           (r  , c+1)
  , (r+1, c-1), (r+1, c), (r+1, c+1)
  ]

isFree :: Input -> Point -> Direction -> Bool
isFree input (r, c) North = not $ any (`M.member` input) [(r-1, c-1), (r-1, c), (r-1, c+1)]
isFree input (r, c) South = not $ any (`M.member` input) [(r+1, c-1), (r+1, c), (r+1, c+1)]
isFree input (r, c) East  = not $ any (`M.member` input) [(r-1, c+1), (r ,c+1), (r+1, c+1)]
isFree input (r, c) West  = not $ any (`M.member` input) [(r-1, c-1), (r ,c-1), (r+1, c-1)]

move :: Point -> Direction -> Point
move (r, c) North = (r - 1, c)
move (r, c) South = (r + 1, c)
move (r, c) East  = (r, c + 1)
move (r, c) West  = (r, c - 1)
move point  None  = point

elf :: Elf
elf = Elf (S.fromList [North, South, West, East])

parseInput :: String -> Input
parseInput = M.map (const elf) . fromRight (error "Unable to parse input") . parseOnly parseInput' . pack
  where
    parseInput' = do
      lines <- line `sepBy` endOfLine
      return $ M.fromList $ filter (\(_, c) -> c /= '.') $ concatMap (\(r,rs) -> map (\(c, v) -> ((r,c),v)) rs) (zip [0..] lines)

    line = do
      lines <- many' $ choice [char '#', char '.']
      return $ zip [0..] lines