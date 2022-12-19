module Day19 (solve1, solve2, parseInput, parSolve1, parSolve2, Input, Output) where

import Data.Attoparsec.Text (decimal, parseOnly, string, sepBy, endOfLine, parse)
import Data.Text (pack)
import Data.Either (fromRight)
import Data.List (sortOn, find)
import Data.Maybe (fromJust)
import Control.Monad.Memo
import Control.Parallel.Strategies

import qualified Data.Sequence as Sq
import qualified Data.Set as S

import Debug.Trace

data Blueprint = Blueprint Int Int Int Int Int Int Int deriving (Show, Eq)
data Robot = None | Ore | Clay | Obsidian | Geode deriving (Show, Eq)
type Inventory = (Int, Int, Int, Int)
type Robots = (Int, Int, Int, Int)
data State = State Inventory Robots Int deriving (Show, Eq, Ord)

type Input = [Blueprint]
type Output = Int

-- 1638 too low
-- 1639 too low
solve1 :: Input -> Output
solve1 = sum . map ((\(l,Blueprint id _ _ _ _ _ _) -> l * id) . run)
  where
    run blueprint = simulate blueprint (maxOreRobots blueprint) 0 S.empty (Sq.singleton $ initialState 24)


solve2 :: Input -> Output
solve2 = sum . map ((\(l,Blueprint id _ _ _ _ _ _) -> l * id) . run) . take 3
  where
    run blueprint = simulate blueprint (maxOreRobots blueprint) 0 S.empty (Sq.singleton $ initialState 32)

parSolve1 :: Input -> Output
parSolve1 i = sum $ parMap rdeepseq run i
  where
    run blueprint@(Blueprint id _ _ _ _ _ _)  = id * fst (simulate blueprint (maxOreRobots blueprint) 0 S.empty (Sq.singleton $ initialState 24))

parSolve2 :: Input -> Output
parSolve2 i = product $ parMap rdeepseq run (take 3 i)
  where
    run blueprint = fst (simulate blueprint (maxOreRobots blueprint) 0 S.empty (Sq.singleton $ initialState 32))


{--
simulate :: Blueprint -> Int -> State -> Int
simulate blueprint maxOreRobots initialState = startEvalMemo $ simulateM initialState
  where
    simulateM :: State -> Memo State Int Int
    simulateM (State (_, _, _, geo) _ 0) = return 0
    simulateM state@(State inv _ _) = do
      states' <- mapM (memo simulateM) states
      return $ maximum states'
      where
        state' = produce state
        -- order by priority queue ?
        states = map (build state' blueprint) . filter (canBuild inv blueprint) $ buildOrder state maxOreRobots
--}

simulate :: Blueprint -> Int -> Int -> S.Set State -> Sq.Seq State -> (Int, Blueprint)
simulate blueprint _ maxGeos cache Sq.Empty = (maxGeos, blueprint)
simulate blueprint maxOreRobots maxGeos cache (state@(State inv@(_, _, _, geos) robots time) Sq.:<| states)
  | geos < maxGeos' - 2 || State inv robots 0 `S.member` cache || time == 0 = simulate blueprint maxOreRobots maxGeos' cache  states
  | otherwise                                                               = simulate blueprint maxOreRobots maxGeos' cache' states'
  where
    maxGeos' = max maxGeos geos
    cache'   = State inv robots 0 `S.insert` cache
    state'   = produce state
    states'
      | canBuild inv blueprint Geode = states Sq.|> build state' blueprint Geode
      | otherwise                    = foldl (Sq.|>) states $ map (build state' blueprint) possibleRobots
      where
        possibleRobots = filter (canBuild inv blueprint) (buildOrder state maxOreRobots)

canBuild :: Inventory -> Blueprint -> Robot -> Bool
canBuild _ _ None = True
canBuild (ore,  _,  _, _ ) (Blueprint _ cC _ _ _ _ _ ) Ore = ore >= cC
canBuild (ore,  _,  _, _ ) (Blueprint _ _ cC _ _ _ _ ) Clay = ore >= cC
canBuild (ore, cl,  _, _ ) (Blueprint _ _ _ oC cC _ _) Obsidian = ore >= oC && cl >= cC
canBuild (ore, _ , ob, _ ) (Blueprint _ _ _ _ _ orC obC) Geode = ore >= orC && ob >= obC

produce :: State -> State
produce (State (oR, cl, ob, geo) robots@(rOr, rCl, rOb, rGe) time) = State inv' robots (time - 1)
  where
    inv' = (oR + rOr, cl + rCl, ob + rOb, geo+ rGe)

build :: State -> Blueprint -> Robot -> State
build (State (oR, cl, ob, geo) (rOr, rCl, rOb, rGe) time) (Blueprint _ cOr cCl cOO cCO cRG cOG) robot
  | robot == Ore      = State (oR - cOr, cl, ob, geo) (rOr + 1, rCl, rOb, rGe) time
  | robot == Clay     = State (oR - cCl, cl, ob, geo) (rOr, rCl + 1, rOb, rGe) time
  | robot == Obsidian = State (oR - cOO, cl - cCO, ob, geo) (rOr, rCl, rOb + 1, rGe) time
  | robot == Geode    = State (oR - cRG, cl, ob - cOG, geo) (rOr, rCl, rOb, rGe + 1) time
  | robot == None     = State (oR, cl, ob, geo) (rOr, rCl, rOb, rGe) time

initialState :: Int -> State
initialState = State (0, 0, 0, 0) (1, 0, 0, 0)

buildOrder :: State -> Int -> [Robot]
buildOrder (State _ (oreRobots, _, _, _) _) maxOreRobots
  | oreRobots < maxOreRobots = [Ore, Clay, Obsidian, None]
  | otherwise                = [Clay, Obsidian, None]

-- Maybe works?
maxOreRobots :: Blueprint -> Int
maxOreRobots (Blueprint _ r0 r1 r2 _ r3 _ ) = maximum [r0, r1, r2, r3]

-- Blueprint 1: Each ore robot costs 2 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 15 clay. Each geode robot costs 2 ore and 15 obsidian.
parseInput :: String -> Input
parseInput = fromRight (error "Unable to parse input") . parseOnly (blueprint `sepBy` endOfLine) . pack

-- Blueprint 30: Each ore robot costs 3 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 18 clay. Each geode robot costs 3 ore and 8 obsidian.
blueprint = do
  _        <- string (pack "Blueprint ")
  b        <- decimal
  _        <- string (pack ": Each ore robot costs ")
  oreCost  <- decimal
  _        <- string (pack " ore. Each clay robot costs ")
  clayCost <- decimal
  _        <- string (pack " ore. Each obsidian robot costs ")
  oOreCst  <- decimal
  _        <- string (pack " ore and ")
  oClyCst  <- decimal
  _        <- string (pack " clay. Each geode robot costs ")
  gOreCst  <- decimal
  _        <- string (pack " ore and ")
  gObsCst  <- decimal
  _        <- string (pack " obsidian.")
  return $ Blueprint b oreCost clayCost oOreCst oClyCst gOreCst gObsCst


-----------------------------------------------------------------------------------------------------------

i :: [Blueprint]
i =
  [ Blueprint 1 4 2 3 14 2 7
  , Blueprint 2 2 3 3 8 3 12
  ]