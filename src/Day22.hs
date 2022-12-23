module Day22 (solve1, solve2, parseInput, Input, Output) where

import Data.Attoparsec.Text (decimal, parseOnly, many', endOfLine, sepBy, takeTill, anyChar, parse, choice, takeWhile1)
import Data.Text (pack, unpack)
import Data.Either (fromRight)
import Data.List (find)
import Data.Maybe (fromJust)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Array as A

import Debug.Trace

type Point = (Int, Int)
type Map = M.Map Int (M.Map Int Char)

type Path = [PathElement]
data PathElement = Move Int | L | R deriving (Show, Eq)

type Input = (Path, Map)
type Output = Int

solve1 :: Input -> Output
solve1 (path, input) = (1000 * (row + 1)) + (4 * (col + 1)) + facingValue
  where
    ((row, col), dir) = moveOnMap input path start initialDirection
    start             = (S.findMin $ M.keysSet input, S.findMin $ M.keysSet $ snd $ M.findMin input)
    facingValue       = case dir of
       (0, 1) -> 0
       (0,-1) -> 2
       ( 1,0) -> 1
       (-1,0) -> 3

solve2 :: Input -> Output
solve2  (path, input) = (1000 * (row + 1)) + (4 * (col + 1)) + facingValue
   where
     ((row, col), dir) = moveOnCube input path start initialDirection
     start             = (S.findMin $ M.keysSet input, S.findMin $ M.keysSet $ snd $ M.findMin input)
     facingValue       = case dir of
        (0, 1) -> 0
        (0,-1) -> 2
        ( 1,0) -> 1
        (-1,0) -> 3

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

-- NOTE: seams are hardcoded, may not work with other cubes that are not unwrapped like this
--        Top    Left
--        Front
--  Right Bottom
--  Back
moveOnCube :: Map -> Path -> Point -> Point -> (Point, Point)
moveOnCube input []            point dir    = (point, dir)
moveOnCube input ((Move 0):ms) point dir    = moveOnCube input ms point dir
moveOnCube input (L:ms)        point (0, 1) = moveOnCube input ms point (-1, 0)
moveOnCube input (L:ms)        point (0,-1) = moveOnCube input ms point ( 1, 0)
moveOnCube input (L:ms)        point ( 1,0) = moveOnCube input ms point ( 0, 1)
moveOnCube input (L:ms)        point (-1,0) = moveOnCube input ms point ( 0,-1)
moveOnCube input (R:ms)        point (0, 1) = moveOnCube input ms point ( 1, 0)
moveOnCube input (R:ms)        point (0,-1) = moveOnCube input ms point (-1, 0)
moveOnCube input (R:ms)        point ( 1,0) = moveOnCube input ms point ( 0,-1)
moveOnCube input (R:ms)        point (-1,0) = moveOnCube input ms point ( 0, 1)
moveOnCube input ((Move n):ms) point@(r, c) dir@(dR, dC)
  | isOnMap tile && isWall tile = moveOnCube input ms point dir
  | isOnMap tile                = moveOnCube input (Move (n-1):ms) target dir
  | isWall warpTile             = moveOnCube input ms point dir
  | otherwise                   = moveOnCube input (Move (n-1):ms) warpPos warpDir
  where
    target  = (r + dR, c + dC)
    tile    = M.findWithDefault ' ' (c + dC) $ M.findWithDefault M.empty (r + dR) input
    isOnMap = (/=) ' '
    isWall  = (==) '#'
    warpTile = M.findWithDefault ' ' (snd warpPos) $ M.findWithDefault M.empty (fst warpPos) input
    (warpPos, warpDir) = warp
    rows = fst $ M.findMax input
    warp
      | dR == -1 && r ==   0   && A.inRange ( 50, 100-1) c = ((c + 100, 0)       , ( 0, 1))
      | dR == -1 && r ==   0   && A.inRange (100, 150-1) c = ((200-1, c - 100)   , ( -1, 0))
      | dR == -1 && r == 100   && A.inRange (  0,  50-1) c = ((c + 50, 50)       , ( 0, 1))
      | dC ==  1 && c == 150-1 && A.inRange (  0,  50-1) r = ((150-1 - r, 100-1) , ( 0,-1))
      | dC ==  1 && c == 100-1 && A.inRange ( 50, 100-1) r = ((50-1, r + 50)     , (-1,  0))
      | dC ==  1 && c == 100-1 && A.inRange (100, 150-1) r = ((150-1 - r, 150-1) , ( 0, -1))
      | dC ==  1 && c ==  50-1 && A.inRange (150, 200-1) r = ((150-1, r - 100)   , (-1, 0))
      | dR ==  1 && r ==  50-1 && A.inRange (100, 150-1) c = ((c - 50, 100-1)    , ( 0, -1))
      | dR ==  1 && r == 150-1 && A.inRange ( 50, 100-1) c = ((100 + c, 50-1)    , ( 0, -1))
      | dR ==  1 && r == 200-1 && A.inRange (  0,  50-1) c = ((0, c + 100)       , ( 1,  0))
      | dC == -1 && c ==   0   && A.inRange (150, 200-1) r = ((0, r - 100)       , ( 1,  0))
      | dC == -1 && c ==   0   && A.inRange (100, 150-1) r = ((150-1 - r, 50)    , ( 0,  1))
      | dC == -1 && c ==  50   && A.inRange ( 50, 100-1) r = ((100, r - 50)      , ( 1,  0))
      | dC == -1 && c ==  50   && A.inRange (  0,  50-1) r = ((150-1 - r, 0)     , ( 0,  1))

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
      return (moves, M.fromList $ zip [0..] lines)

    line = do
      tiles <- takeWhile1 (/= '\n')
      return $ M.fromList $ filter (\(_, v) -> v /= ' ') $ zip [0..] $ unpack tiles

    direction = do
      dir <- anyChar
      return (case dir of
                 'L' -> L
                 'R' -> R)

    movements = Move <$> decimal