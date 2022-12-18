module Day18 (solve1, solve2, parseInput, Input, Output) where

import Data.Attoparsec.Text (decimal, parseOnly, sepBy, char, endOfLine)
import Data.Text (pack)
import Data.Either (fromRight)

import qualified Data.Set as S
import qualified Data.Array as A

type Point = (Int, Int, Int)

type Input = S.Set Point
type Output = Int

solve1 :: Input -> Output
solve1 input = foldl (\c e -> c + countFreeFaces input e) 0 (S.elems input)

countFreeFaces :: Input -> Point -> Int
countFreeFaces input (x,y,z) = foldl isFree 0 ps
  where
    isFree c point = if point `S.member` input then c else c + 1
    ps = [ (x-1, y, z), (x+1, y, z), (x, y-1, z), (x, y+1, z), (x, y, z-1), (x, y, z+1) ]

solve2 :: Input -> Output
solve2 input = freeFaces - foldl (\c p -> c + countCoveredFaces input p) 0 (S.elems interior)
  where
    freeFaces = foldl (\c e -> c + countFreeFaces input e) 0 (S.elems input)
    interior  = interiorAirPoints input

airPoints :: Input -> [Point] -> S.Set Point -> S.Set Point
airPoints _ [] visited = visited
airPoints input (point:ps) visited
  | point `S.member` visited = airPoints input ps visited
  | otherwise                = airPoints input (ps ++ freeFaces input point) (point `S.insert` visited)


interiorAirPoints :: Input -> S.Set Point
interiorAirPoints input = S.filter (`S.notMember` air) (allFreeFaces input)
  where
    air = airPoints input (freeFaces input $ S.findMin input) S.empty

freeFaces :: Input -> Point -> [Point]
freeFaces input (x,y,z) = filter (\p -> p `S.notMember` input && A.inRange bounds p) ps
  where
    ps = [ (x-1, y, z), (x+1, y, z), (x, y-1, z), (x, y+1, z), (x, y, z-1), (x, y, z+1) ]

allFreeFaces :: Input -> S.Set Point
allFreeFaces input = foldl (flip S.insert) S.empty $ concatMap (freeFaces input) (S.elems input)

freeFaces' :: Input -> Point -> [Point]
freeFaces' input (x,y,z) = filter (`S.notMember` input) ps
  where
    ps = [ (x-1, y, z), (x+1, y, z), (x, y-1, z), (x, y+1, z), (x, y, z-1), (x, y, z+1) ]

countCoveredFaces :: Input -> Point -> Int
countCoveredFaces input (x,y,z) = foldl isFree 0 ps
  where
    isFree c point = if point `S.member` input then c + 1 else c
    ps = [ (x-1, y, z), (x+1, y, z), (x, y-1, z), (x, y+1, z), (x, y, z-1), (x, y, z+1) ]

parseInput :: String -> Input
parseInput = S.fromList . fromRight (error "Unable to parse input") . parseOnly (line `sepBy` endOfLine) . pack
  where
    line = do
      x <- decimal
      _ <- char ','
      y <- decimal
      _ <- char ','
      z <- decimal
      return (x,y,z)

-- TODO: calculate right bounds
bounds :: (Point, Point)
bounds = ((0,0,0),(22,22,22))

i :: S.Set Point
i = S.fromList
  [(2,2,2)
  ,(1,2,2)
  ,(3,2,2)
  ,(2,1,2)
  ,(2,3,2)
  ,(2,2,1)
  ,(2,2,3)
  ,(2,2,4)
  ,(2,2,6)
  ,(1,2,5)
  ,(3,2,5)
  ,(2,1,5)
  ,(2,3,5)
  ]