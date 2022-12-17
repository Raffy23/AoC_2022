module Main (main) where

import System.Environment (getArgs)
import Data.List (unlines)

import qualified Data.Map as Map

import Common (file)
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day15
import qualified Day16


days :: Map.Map [String] (IO String)
days =
  Map.fromList
    [ (["01", "1"], run (file "01" "1") Day01.parseInput Day01.solve1 show)
    , (["01", "2"], run (file "01" "1") Day01.parseInput Day01.solve2 show)
    , (["02", "1"], run (file "02" "1") Day02.parseInput Day02.solve1 show)
    , (["02", "2"], run (file "02" "1") Day02.parseInput Day02.solve2 show)
    , (["03", "1"], run (file "03" "1") Day03.parseInput Day03.solve1 show)
    , (["03", "2"], run (file "03" "1") Day03.parseInput Day03.solve2 show)
    , (["04", "1"], run (file "04" "1") Day04.parseInput Day04.solve1 show)
    , (["04", "2"], run (file "04" "1") Day04.parseInput Day04.solve2 show)
    , (["05", "1"], run (file "05" "1") Day05.parseInput Day05.solve1 id  )
    , (["05", "2"], run (file "05" "1") Day05.parseInput Day05.solve2 id  )
    , (["06", "1"], run (file "06" "1") Day06.parseInput Day06.solve1 show)
    , (["06", "2"], run (file "06" "1") Day06.parseInput Day06.solve2 show)
    , (["07", "1"], run (file "07" "1") Day07.parseInput Day07.solve1 show)
    , (["07", "2"], run (file "07" "1") Day07.parseInput Day07.solve2 show)
    , (["08", "1"], run (file "08" "1") Day08.parseInput Day08.solve1 show)
    , (["08", "2"], run (file "08" "1") Day08.parseInput Day08.solve2 show)
    , (["09", "1"], run (file "09" "1") Day09.parseInput Day09.solve1 show)
    , (["09", "2"], run (file "09" "1") Day09.parseInput Day09.solve2 show)
    , (["10", "1"], run (file "10" "1") Day10.parseInput Day10.solve1 show)
    , (["10", "2"], run (file "10" "1") Day10.parseInput Day10.solve2 unlines)
    , (["11", "1"], run (file "11" "1") Day11.parseInput Day11.solve1 show)
    , (["11", "2"], run (file "11" "1") Day11.parseInput Day11.solve2 show)
    , (["15", "2"], run (file "15" "1") Day15.parseInput Day15.solve2 show)
    , (["16", "2"], run (file "16" "1") Day16.parseInput Day16.solve2 show)
    ]

runDay :: [String] -> IO ()
runDay [day, part] = case Map.lookup [day, part] days of
  Just f   -> do 
    output <- f
    _      <- putStrLn $ "----[ Day " ++ day ++ ", Part " ++ part ++ " ]----" -- "————[ Day " ++ day ++ ", Part " ++ part ++ " ]————"
    _      <- putStrLn output
    return ()
  Nothing  -> putStrLn $ "Day " ++ day ++ " Part " ++ part ++ " is not implemented!"

run :: String -> (String -> a) -> (a -> b) -> (b -> String) -> IO String
run file parse solve print =  fmap (print . solve . parse) (readFile file)

main :: IO ()
main = do
  args <- getArgs
  if null args
      then mapM_ runDay $ Map.keys days
      else runDay args


