module Day16 (solve1, solve2, parseInput, Input, Output) where

import Data.Attoparsec.Text (decimal, parseOnly, endOfLine, sepBy, string, takeTill, choice)
import Data.Text (Text, pack)
import Data.Either (fromRight)
import Data.Char (isSpace)
import Control.Monad.Memo
import Data.Bifunctor (bimap)

import qualified Data.Attoparsec.Text as P (take)
import qualified Data.IntSet as IS
import qualified Data.Map as M

type Input =  M.Map Int INode
type Output = Int
type Distances = M.Map (Int, Int) Int

data Node  = Node  Int [Text]    deriving (Show, Eq)
data INode = INode Int IS.IntSet deriving (Show, Eq)

solve1 :: Input -> Output
solve1 input = search input 30 False

solve2 :: Input -> Output
solve2 input = search input 26 True

search :: Input -> Int -> Bool -> Int
search input time elephant = startEvalMemo $ searchM (valves, 1, time, elephant)
  where
    valves = workingValves input
    dist   = distances input

    searchM :: (IS.IntSet, Int, Int, Bool) -> Memo (IS.IntSet, Int, Int, Bool) Int Int
    searchM (nodes, node, time, elephant) = do
      results <- sequence [ value other | other <- IS.elems nodes, canVisit other ]
      eResult <- if elephant then memo searchM (nodes, 1, 26, False) else return 0
      return $ maximum ( eResult : results )
      where
        canVisit other = dist M.! (node, other) < time
        value other = do
          result <- memo searchM (IS.delete other nodes, other, dT, elephant)
          return $ pressure (input M.! other) * dT + result
          where
            dT = time - dist M.! (node, other) - 1

workingValves :: Input -> IS.IntSet
workingValves = IS.fromList . map fst. M.toList . M.filter (\(INode v _) -> v > 0)

-- Floyd–Warshall algorithm
distances :: Input -> Distances
distances input = foldl updateDist initialDist [(k,i,j) | k <- nodes, i <- nodes, j <- nodes]
  where
    nodes = M.keys input
    initialDist = M.fromList (concatMap weight (M.toList input))
    weight (node, INode _ neighbours) = ((node, node), 0) : map (\n -> ((node, n), 1)) (IS.elems neighbours)
    updateDist dist (k, i, j)
      | (i,k) `M.notMember` dist || (k,j) `M.notMember` dist = dist
      | (i,j) `M.notMember` dist                             = M.insert (i,j) (dist M.! (i,k) + dist M.! (k,j)) dist
      | dist M.! (i,j) > dist M.! (i,k) + dist M.! (k,j)     = M.insert (i,j) (dist M.! (i,k) + dist M.! (k,j)) dist
      | otherwise                                            = dist

parseInput :: String -> Input
parseInput = convertInput . M.fromList . fromRight (error "Unable to parse input") . parseOnly (line `sepBy` endOfLine) . pack
  where
    line = do
      _         <- string (pack "Valve ")
      valve     <- takeTill isSpace
      _         <- string (pack " has flow rate=")
      flow      <- decimal
      _         <- choice [string (pack "; tunnels lead to valves "), string (pack "; tunnel leads to valve ")]
      neighbors <- P.take 2 `sepBy` string (pack ", ")
      return (valve, Node flow neighbors)

pressure :: INode -> Int
pressure (INode p _ ) = p

convertInput :: M.Map Text Node -> Input
convertInput input = M.fromList . map (bimap (keys M.!) node) $ M.toList input
  where
    keys :: M.Map Text Int
    keys = M.fromList $ zip (M.keys input) [1..]
    node (Node p ns) = INode p $ IS.fromList $ map (keys M.!) ns
