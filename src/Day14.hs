module Day14 (solve1, solve2, parseInput, Input, Output) where

import Data.Attoparsec.Text (decimal, parseOnly, string, sepBy, char, endOfLine)
import Data.Text (pack)
import Data.Either (fromRight)
import Data.Maybe
import Data.List (findIndices, minimumBy, maximumBy)
import qualified Data.Array as A
import qualified Data.Set as S

type Point = (Int, Int)
type ScanLine = [Point]
type Bounds = (Point, Point)

type Input = [ScanLine]
type Output = Int

solve2 :: Input -> Output
solve2 i = spawnSand' points (y+2) 0
 where
   points     = S.fromList (concatMap materializeScanLine i)
   (_, (x,y)) = scanLinesBounds i

solve1 :: Input -> Int
solve1 i = spawnSand points bounds 0
  where
    points = S.fromList (concatMap materializeScanLine i)
    bounds = scanLinesBounds i

parseInput :: String -> Input
parseInput = fromRight (error "Unable to parse input") . parseOnly (scanLine `sepBy` endOfLine) . pack
  where
    scanLine = point `sepBy` string (pack " -> ")
    point = do
      x <- decimal
      _ <- char ','
      y <- decimal
      return (x, y)

i :: Input
i = [[(498,4),(498,6),(496,6)],[(503,4),(502,4),(502,9),(494,9)]]

-- can be Set; only info needed if exists for part 1
points = S.fromList (concatMap materializeScanLine i)
bounds = scanLinesBounds i

spawnSand :: S.Set Point -> Bounds -> Int -> Int
spawnSand points bounds count = case fall points bounds (500, 0) of
  Just point -> spawnSand (S.insert point points) bounds (count + 1)
  Nothing    -> count

spawnSand' :: S.Set Point -> Int -> Int -> Int
spawnSand' points lineY count = case fall' points lineY (500, 0) of
  Just point -> spawnSand' (S.insert point points) lineY (count + 1)
  Nothing    -> count

fall :: S.Set Point -> Bounds -> Point -> Maybe Point
fall points bounds point
  | S.member point points = Nothing
  | otherwise             = case step points bounds point of
      Just point' -> if point' /= point then fall points bounds point' else Just point'
      Nothing     -> Nothing

fall' :: S.Set Point -> Int -> Point -> Maybe Point
fall' points lineY point
  | S.member point points = Nothing
  | otherwise             = case step' points lineY point of
      Just point' -> if point' /= point then fall' points lineY point' else Just point'
      Nothing     -> Nothing

step :: S.Set Point -> Bounds -> Point -> Maybe Point
step points bounds (x, y)
  | S.notMember (x, y + 1)     points = inRange (x    , y + 1)
  | S.notMember (x - 1, y + 1) points = inRange (x - 1, y + 1)
  | S.notMember (x + 1, y + 1) points = inRange (x + 1, y + 1)
  | otherwise                         = Just (x,y)
  where
    inRange point = case A.inRange bounds point of
      True  -> Just point
      False -> Nothing

step' :: S.Set Point -> Int -> Point -> Maybe Point
step' points lineY (x, y)
  | y + 1 < lineY = step
  | otherwise     = Just (x, y)
  where
    step
      | S.notMember (x, y + 1)     points = Just (x    , y + 1)
      | S.notMember (x - 1, y + 1) points = Just (x - 1, y + 1)
      | S.notMember (x + 1, y + 1) points = Just (x + 1, y + 1)
      | otherwise                         = Just (x, y)

materializeScanLine :: ScanLine -> [Point]
materializeScanLine [_] = []
materializeScanLine (s : e : ps) = materializePoints s e ++ materializeScanLine (e:ps)

materializePoints :: Point -> Point -> [Point]
materializePoints (sX, sY) (eX, eY)
  | sX == eX && sY == eY = [(eX, eY)]
  | otherwise            = (sX, sY) : materializePoints (sX + diffX, sY + diffY) (eX, eY)
  where
    diffX = signum (eX - sX)
    diffY = signum (eY - sY)

scanLinesBounds :: [ScanLine] -> (Point, Point)
scanLinesBounds input = ((minX, minY), (maxX, maxY))
  where
    xS = concatMap (map fst) input
    yS = concatMap (map snd) input
    minX = minimum xS
    maxX = maximum xS
    minY = 0 -- Sand spawns at 500,0
    maxY = maximum yS
