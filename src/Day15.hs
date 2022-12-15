module Day15 (solve1, solve2, parseInput, Input, Output) where

import Data.Attoparsec.Text (decimal, Parser, parseOnly, many', endOfLine, signed, string, sepBy)
import Data.Text (pack)
import Data.Either (fromRight)
import Data.List (sortOn, find, minimumBy, maximumBy, sortBy)
import Data.Ord
import qualified Data.Set as S
import Debug.Trace
import Data.Function (on)
import qualified Data.RangeSet.List as RSet

type Point = (Int, Int)
type Sensor = Point
type Beacon = Point


type Input = [(Sensor, Beacon)]
type Output = Int

solve1 :: Input -> Output
solve1 i = abs (min - max)
  where
    y   = 2000000
    d   = distancesY y $ distances i
    d1  = filter isInRange d
    v   = RSet.fromRangeList $ map asRangeX d1
    min = RSet.findMin v
    max = RSet.findMax v


-- TODO: Parse the output
-- TODO: Check bounds of i before iterating on it
solve2 :: Input -> Maybe (Int, RSet.RSet Int)
solve2 i = find (\(i, set) -> length (RSet.toRangeList set) == 2) ranges
  where
    d = distances i
    rangeSet y = RSet.fromRangeList $ map asRangeX $ filter isInRange $ distancesY y d
    ranges = [ (i, rangeSet i) | i <- [0 .. 4000000] ]

-- Sensor at x=2, y=18: closest beacon is at x=-2, y=15
parseInput :: String -> Input
parseInput = fromRight (error "Unable to parse input") . parseOnly (line `sepBy` endOfLine) . pack
  where
    line = do
      _ <- string (pack "Sensor at ")
      s <- point
      _ <- string (pack ": closest beacon is at ")
      b <- point
      return (s, b)

    point = do
      _ <- string (pack "x=")
      x <- signed decimal
      _ <- string (pack ", y=")
      y <- signed decimal
      return (x,y)

dist :: Point -> Point -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

distances :: Input -> [(Point, Point, Int)]
distances = map (\(s,b) -> (s,b,dist s b))

distancesY :: Int -> [(Point, Point, Int)] -> [((Point, Point, Int), Int)]
distancesY _ [] = []
distancesY y ((s@(_, sY), b, d):rest) = ((s, b, d), abs (sY - y)) : distancesY y rest

asRangeX :: ((Point, Point, Int), Int) -> (Int, Int)
asRangeX (((x, _), _, d), dY) = (start, end)
  where
    start = x - (d - dY)
    end   = x + (d - dY)

isInRange :: ((Point, Point, Int), Int) -> Bool
isInRange ((_, _, d), dY) = d >= dY
