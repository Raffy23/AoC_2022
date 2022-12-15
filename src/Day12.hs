{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day12 (solve1, solve2, parseInput, Input, Output) where

import qualified Data.Array as A
import qualified Data.Set as S

import Data.Char

type Point = (Int, Int)
type Dist = (Point, Int)
type Graph = A.Array Point Char

type Input = A.Array Point Char
type Output = Int

solve1 :: Input -> Output
solve1 graph = bfs S.empty [(startPoint, 0)] graph 'S'
 where
    startPoint = fst . head . filter ((== 'E') . snd) $ A.assocs graph

solve2 :: Input -> Output
solve2 graph = bfs S.empty [(startPoint, 0)] graph 'a'
 where
    startPoint = fst . head . filter ((== 'E') . snd) $ A.assocs graph

bfs :: S.Set Point -> [Dist] -> Graph -> Char -> Int
bfs visited ((start, dist):rest) graph end
  | graph A.! start == end = dist
  | S.member start visited = bfs visited  rest  graph end
  | otherwise              = bfs visited' rest' graph end
  where
    neighbours' = neighbours graph start
    visited'   = S.insert start visited
    new        = filter (`S.notMember` visited) neighbours'
    rest'      = error "" --rest ++ map (,dist+1) new

neighbours :: Graph -> Point -> [Point]
neighbours graph point@(x,y) =  filter canMoveTo neighbours'
  where
    canMoveTo p = height' - height (graph A.! p) < 2
    height'     = height $ graph A.! point
    neighbours' = filter (A.inRange $ A.bounds graph) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

parseInput :: String -> Input
parseInput s = A.listArray ((0,0), (x-1, y-1)) (concat chars) where
    chars  = lines s
    x = length chars
    y = length (head chars)

height :: Char -> Int
height 'S' = ord 'a'
height 'E' = ord 'z'
height  c  = ord c