module Day06 (solve1, solve2, parseInput, Input, Output) where

import qualified Data.Set as S

type Input = String
type Output = Int

solve1 :: Input -> Output
solve1 = findMarker 4

findMarker :: Int -> String -> Int
findMarker pos (m1:m2:m3:m4:ms)
  | m1Uniq && m2Uniq && m3Uniq = pos
  | otherwise                  = findMarker (pos + 1) (m2:m3:m4:ms)
  where
    m1Uniq = m1 /= m2 && m1 /= m3 && m1 /= m4
    m2Uniq =             m2 /= m3 && m2 /= m4
    m3Uniq = m1 /= m3 &&             m3 /= m4

solve2 :: Input -> Output
solve2 = findMarker2 14

findMarker2 :: Int -> String -> Int
findMarker2 pos str
  | isUnique $ take 14 str = pos
  | otherwise              = findMarker2 (pos + 1) $ drop 1 str

isUnique :: [Char] -> Bool
isUnique list = length list == length set
  where set = S.fromList list

parseInput :: String -> Input
parseInput = id