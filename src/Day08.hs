module Day08 (solve1, solve2, parseInput, Input, Output) where

import Data.Attoparsec.Text (Parser, parseOnly, many1', endOfLine, digit, choice, endOfInput)
import Data.Text (pack)
import Data.Either (fromRight)
import Data.Char (digitToInt)
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Maybe

type Input = M.Matrix Int
type Output = Int

example :: M.Matrix Int
example = M.fromLists [ 
  [3,0,3,7,3],
  [2,5,5,1,2],
  [6,5,3,3,2],
  [3,3,5,4,9],
  [3,5,3,9,0]]

solve1 :: Input -> Output
solve1 inMatrix = V.foldl (\c b -> if b then c+1 else c) 0 $ M.getMatrixAsVector visibleMatrix
  where
    visibleMatrix = M.mapPos (\pos _ -> isVisible pos inMatrix) inMatrix

solve2 :: Input -> Output
solve2 inMatrix = V.maximum $ M.getMatrixAsVector visibleMatrix
  where
    visibleMatrix = M.mapPos (\pos _ -> treeScore pos inMatrix) inMatrix

parseInput :: String -> Input
parseInput = M.fromLists . fromRight (error "Unable to parse input") . parseOnly parseInput' . pack
  where
    parseInput' :: Parser [[Int]]
    parseInput' = do
      lines <- many1' line
      last  <- many1' $ digitToInt <$> digit
      _     <- endOfInput
      return $ lines ++ [last]
  
    line = do
      trees <- many1' $ digitToInt <$> digit
      _     <- endOfLine
      return trees

splitAt' :: Int -> V.Vector a -> (V.Vector a, V.Vector a)
splitAt' n vec = (V.take (n-1) vec, V.drop n vec)

isVisible :: (Int, Int) -> M.Matrix Int -> Bool
isVisible (row,col) m = canBeSeen left || canBeSeen right || canBeSeen top || canBeSeen bottom
  where
    current = M.unsafeGet row col m
    (left, right) = splitAt' col $ M.getRow row m
    (top, bottom) = splitAt' row $ M.getCol col m

    canBeSeen vec = (V.length vec == 0) || isNothing (V.findIndex (>= current) vec)

treeScore :: (Int, Int) -> M.Matrix Int -> Int
treeScore (row,col) m =  score' up * score' left * score' right * score' down
  where
    current = M.unsafeGet row col m
    (leftR, right) = splitAt' col $ M.getRow row m
    (upR, down)    = splitAt' row $ M.getCol col m
    left = V.reverse leftR
    up   = V.reverse upR

    score' vec = score vec current 0 0

score :: V.Vector Int -> Int -> Int -> Int -> Int
score vec cur idx count
  | V.length vec == 0   = 0
  | V.length vec == idx = count
  | vec V.! idx  <  cur = score vec cur (idx+1) (count+1)
  | vec V.! idx  == cur = count+1
  | otherwise           = count+1