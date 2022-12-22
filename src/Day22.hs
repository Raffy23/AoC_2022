module Day22 (solve1, solve2, parseInput, Input, Output) where

import Data.Attoparsec.Text (decimal, parseOnly, many', endOfLine, sepBy, takeTill, anyChar, parse, choice, takeWhile1)
import Data.Text (pack, unpack)
import Data.Either (fromRight)
import Data.List (find)
import Data.Maybe (fromJust)

import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace

type Point = (Int, Int)
type Map = M.Map Int (M.Map Int Char)

type Path = [PathElement]
data PathElement = Move Int | L | R deriving (Show, Eq)

type Input = (Path, Map)
type Output = Int

solve1 :: Input -> Output
solve1 (path, input) = (1000 * row) + (4 * col) + facingValue
  where
    ((row, col), dir) = moveOnMap input path start initialDirection
    start             = (S.findMin $ M.keysSet input, S.findMin $ M.keysSet $ snd $ M.findMin input)
    facingValue       = case dir of
       (0, 1) -> 0
       (0,-1) -> 2
       ( 1,0) -> 1
       (-1,0) -> 3

solve2 :: Input -> Output
solve2 input = error ""

moveOnMap :: Map -> Path -> Point -> Point -> (Point, Point)
moveOnMap input []            point dir    = (point, dir)
moveOnMap input ((Move 0):ms) point dir    = moveOnMap input ms point dir
moveOnMap input (L:ms)        point (0, 1) = moveOnMap input ms point (-1, 0)
moveOnMap input (L:ms)        point (0,-1) = moveOnMap input ms point ( 1, 0)
moveOnMap input (L:ms)        point ( 1,0) = moveOnMap input ms point ( 0, 1)
moveOnMap input (L:ms)        point (-1,0) = moveOnMap input ms point ( 0,-1)
moveOnMap input (R:ms)        point (0, 1) = moveOnMap input ms point ( 1, 0)
moveOnMap input (R:ms)        point (0,-1) = moveOnMap input ms point (-1, 0)
moveOnMap input (R:ms)        point ( 1,0) = moveOnMap input ms point ( 0,-1)
moveOnMap input (R:ms)        point (-1,0) = moveOnMap input ms point ( 0, 1)
moveOnMap input ((Move n):ms) point@(r, c) dir@(dR, dC)
  | isOnMap tile && isWall tile = moveOnMap input ms point dir
  | isOnMap tile                = moveOnMap input (Move (n-1):ms) target dir
  | isWall warpTile             = moveOnMap input ms point dir
  | otherwise                   = moveOnMap input (Move (n-1):ms) warpPos dir
  where
    target  = (r + dR, c + dC)
    tile    = M.findWithDefault ' ' (c + dC) $ M.findWithDefault M.empty (r + dR) input
    isOnMap = (/=) ' '
    isWall  = (==) '#'
    warpTile = M.findWithDefault ' ' (snd warpPos) $ M.findWithDefault M.empty (fst warpPos) input
    warpPos
      | dC == -1 = (r, S.findMax $ M.keysSet $ input M.! r)
      | dC ==  1 = (r, S.findMin $ M.keysSet $ input M.! r)
      | dR == -1 = (rows - fromJust (find (\r -> M.member c $ input M.! (rows - r)) [0 ..(rows - 1)]), c)
      | dR ==  1 = (       fromJust (find (\r -> M.member c $ input M.! r)          [1 .. rows     ]), c)
    rows = fst $ M.findMax input

initialDirection :: Point
initialDirection = (0, 1)

parseInput :: String -> Input
parseInput = fromRight (error "Unable to parse input") . parseOnly parseInput' . pack
  where
    parseInput' = do
      lines <- line `sepBy` endOfLine
      _     <- endOfLine
      _     <- endOfLine
      moves <- many' $ choice [movements, direction]
      return (moves, M.fromList $ zip [1..] lines)

    line = do
      tiles <- takeWhile1 (/= '\n')
      return $ M.fromList $ filter (\(_, v) -> v /= ' ') $ zip [1..] $ unpack tiles

    direction = do
      dir <- anyChar
      return (case dir of
                 'L' -> L
                 'R' -> R)

    movements = Move <$> decimal