module Day20 (solve1, solve2, parseInput, Input, Output) where

import Data.Attoparsec.Text (decimal, parseOnly, sepBy, endOfLine, signed)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.List (find)

import qualified Data.Text as T
import qualified Data.Map as M

type Entry = (Int, Int)

type Input = M.Map Int Entry
type Output = Int

solve1 :: Input -> Output
solve1 input = sum . map snd $ filter (\(x, _) -> x == num1Idx || x == num2Idx || x == num3Idx) mixResult
  where
    size      = M.size input
    mixResult = M.elems $ mix input 0 size M.empty
    zeroIdx   = fst $ fromJust $ find (\(_, y) -> y == 0) mixResult
    num1Idx   = (zeroIdx + 1000) `mod` size
    num2Idx   = (zeroIdx + 2000) `mod` size
    num3Idx   = (zeroIdx + 3000) `mod` size

solve2 :: Input -> Output
solve2 input = sum . map snd $ filter (\(x, _) -> x == num1Idx || x == num2Idx || x == num3Idx) mixResult
  where
    input'    = M.map (\(x, value) -> (x, value * 811589153)) input
    size      = M.size input
    mixResult = M.elems $ mixN input' size 10
    zeroIdx   = fst $ fromJust $ find (\(_, y) -> y == 0) mixResult
    num1Idx   = (zeroIdx + 1000) `mod` size
    num2Idx   = (zeroIdx + 2000) `mod` size
    num3Idx   = (zeroIdx + 3000) `mod` size

mix :: Input -> Int -> Int -> Input -> Input
mix input current bounds acc
  | current == bounds = acc
  | otherwise         = mix newMap (current + 1) bounds (M.insert current new newLst)
  where
    old = input M.! current
    new = updateIdx old
    newMap = M.map (mapIndex old new bounds) (M.delete current input)
    newLst = M.map (mapIndex old new bounds) acc

    updateIdx :: Entry -> Entry
    updateIdx (x, value) = ((x + value) `mod` (bounds - 1), value)

mapIndex :: Entry -> Entry -> Int -> Entry -> Entry
mapIndex (oldIdx, oldValue) (newIdx, _) itr (idx, value)
  | idx == newIdx && oldValue > 0    && newIdx >  oldIdx = ((idx - 1) `mod` itr, value)
  | idx == newIdx && oldValue > 0    && newIdx <= oldIdx = ((idx + 1) `mod` itr, value)
  | idx == newIdx && newIdx <  oldIdx                    = ((idx + 1) `mod` itr, value)
  | idx == newIdx && newIdx >= oldIdx                    = ((idx - 1) `mod` itr, value)
  | idx >  newIdx && idx < oldIdx                        = ((idx + 1) `mod` itr, value)
  | idx <  newIdx && idx > oldIdx                        = ((idx - 1) `mod` itr, value)
  | otherwise                                            = (idx                , value)

mixN :: Input -> Int -> Int -> Input
mixN input _   0 = input
mixN input len r = mixN (mix input 0 len M.empty) len (r - 1)

parseInput :: String -> Input
parseInput = M.fromList . zip [0..] . zip [0..] . fromRight (error "Unable to parse input") . parseOnly (signed decimal `sepBy` endOfLine) . T.pack
