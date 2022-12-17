module Day17 (solve1, solve2, parseInput, Input, Output) where

import Data.Attoparsec.Text (parseOnly, many', choice, char)
import Data.Text (pack)
import Data.Either (fromRight)
import Data.List (find)
import Data.Functor (($>))
import Data.Maybe (isJust)

import qualified Data.Set as S
import qualified Data.Map as M

import Debug.Trace

type Input = [Movement]
type Output = Int

data Movement = L | R deriving (Show, Eq)
type Rock  = [Point] -- VBar | Plus | RevL | HBar | Block deriving (Show, Eq)
type Point = (Int, Int)
type Cave  = S.Set Point

instance Ord Movement where
  compare _ _ = EQ

solve1 :: Input -> Output
solve1 i = simulateRocks' (cycle $ zip [0..] rocks) (cycle $ zip [0..] i) M.empty initialCave 0 2022

solve2 :: Input -> Output
solve2 i = simulateRocks' (cycle $ zip [0..] rocks) (cycle $ zip [0..] i) M.empty initialCave 0 1000000000000

parseInput :: String -> Input
parseInput = fromRight (error "Unable to parse input") . parseOnly (many' $ choice [left, right]) . pack
  where
    left  = char '<' $> L
    right = char '>' $> R

initialCave :: S.Set Point
initialCave = S.fromList [(0,i) | i <- [0..6]]

rocks :: [Rock]
rocks =
  [ [(0,0), (0,1), (0,2), (0,3)]
  , [(0,1), (1,0), (1,1), (1,2), (2,1)]
  , [(2,2), (1,2), (0,0), (0,1), (0,2)]
  , [(0,0), (1,0), (2,0), (3,0)]
  , [(0,0), (0,1), (1,0), (1,1)]
  ]

simulateRocks' :: [(Int, Rock)] -> [(Int, Movement)] -> M.Map (Int, Int, [Int]) (Int, Int) -> Cave -> Int -> Int -> Int
simulateRocks' ((_, shape):nR@(nShapeID, _):rest) movements cache cave rockIndex limit
  | rockIndex == limit = fst (S.findMax cave)
  | otherwise          =
  case cacheKey `M.lookup` cache of
    Just (lastIndex, lastMaxHeight) -> calcCycleHeight lastIndex lastMaxHeight
    Nothing                         -> simulateRocks' rest' m' cache' cave' (rockIndex + 1) limit
  where
    rest'       = nR:rest
    (cave', m') = simulateRock' rock movements cave
    rock        = spawnRock shape (fst (S.findMax cave) + 4)

    maxHeight   = fst $ S.findMax cave'
    cacheKey    = (nShapeID, fst (head m'), [ colHeight col | col <- [0..6] ])
    colHeight c = maxHeight - fst (S.findMax $ S.filter (\(_, col) -> c == col) cave')
    cache'      = M.insert cacheKey (rockIndex + 1, maxHeight) cache

    calcCycleHeight lastIndex lastMaxHeight = trace (show remHeight ++ ", " ++ show fastFwdHeight) $ remHeight + fastFwdHeight
      where
        rockIndex'     = rockIndex + 1 -- cave'
        cycleLength    = rockIndex' - lastIndex
        cycleRemaining = (limit - rockIndex') `div` cycleLength
        fastFwdRocks   = rockIndex' + (cycleRemaining * cycleLength)
        fastFwdHeight  = cycleRemaining * (maxHeight - lastMaxHeight)
        remainingRocks = limit - fastFwdRocks
        rocks'         = take remainingRocks $ map snd rest'
        (cave'', _)    = foldl (\(cave,movements) shape -> simulateRock' (spawnRock shape (fst (S.findMax cave) + 4)) movements cave) (cave', m') rocks'
        remHeight      = fst (S.findMax cave'')

simulateRock' :: Rock -> [(Int, Movement)] -> Cave -> (Cave, [(Int, Movement)])
simulateRock' shape ((_,m):ms) cave = case simRockStep shape m cave of
  Left cave'   -> (cave', ms)
  Right shape' -> simulateRock' shape' ms cave

spawnRock :: Rock -> Int -> Rock
spawnRock shape row = map (\(r, col) -> (r+row, col + 2)) shape

simRockStep :: Rock -> Movement -> Cave -> Either Cave Rock
simRockStep shape movement cave = fallRock (pushRock shape movement cave) cave

fallRock :: Rock -> Cave -> Either Cave Rock
fallRock shape cave = if isBlocked then Left cave' else Right shape'
  where
    isBlocked = isJust $ find (`S.member` cave) shape'
    shape'    = map(\(row, col) -> (row - 1, col)) shape
    cave'     = foldl (flip S.insert) cave shape

pushRock :: Rock -> Movement -> S.Set Point -> Rock
pushRock shape L cave = if isBlocked then shape else shape'
  where
    isBlocked = isJust $ find (\pos@(_, col) -> col < 0 || pos `S.member` cave) shape'
    shape'    = map (\(row, col) -> (row, col - 1)) shape
pushRock shape R cave = if isBlocked then shape else shape'
  where
    isBlocked = isJust $ find (\pos@(_, col) -> col >= 7 || pos `S.member` cave) shape'
    shape'    = map (\(row, col) -> (row, col + 1)) shape

-------------------------------------------------------------
{-- For debugging:
caveStr cave = reverse . chunksOf 7 $ map (\pos -> if pos `S.member` cave then '#' else '.') [ (r,c) | r <- [1..(fst (S.findMax cave) + 4)], c <- [0..6]]

showCave :: Cave -> IO ()
showCave cave = do
  forM_ (caveStr cave) putStrLn
--}