module Main (main) where

import qualified Day01
import qualified Day02
import qualified Data.Map as Map
import System.Environment (getArgs)


days :: Map.Map [String] (IO Int)
days =
  Map.fromList
    [ (["01", "1"], run "input/Day01.part1.txt" Day01.parseInput Day01.solve1)
    , (["01", "2"], run "input/Day01.part1.txt" Day01.parseInput Day01.solve2)
    , (["02", "1"], run "input/Day02.part1.txt" Day02.parseInput Day02.solve1)
    , (["02", "2"], run "input/Day02.part1.txt" Day02.parseInput Day02.solve2)
    ]

runDay :: [String] -> IO ()
runDay s = case Map.lookup s days of
  Just f   -> do output <- f ; print output
  Nothing  -> putStrLn $ "Day or part not implemented: " ++ show s

run :: String -> (String -> a) -> (a -> Int) -> IO Int
run file parse solve =  fmap (solve . parse) (readFile file)

main :: IO ()
main = do
  args <- getArgs
  if null args
      then mapM_ runDay $ Map.keys days
      else runDay args


