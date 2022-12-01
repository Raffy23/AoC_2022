module Main (main) where

import qualified Day01 as Day01
import qualified Data.Map as Map
import System.Environment (getArgs)

days =
  Map.fromList
    [ (["01", "1"], (Day01.parseInput, Day01.solve1, "input/Day01.part1.txt"))
    , (["01", "2"], (Day01.parseInput, Day01.solve2, "input/Day01.part1.txt"))
    ]

runDay :: [String] -> IO ()
runDay s = case Map.lookup s days of
  Just (parse, solve, file) -> run file parse solve >> return ()
  Nothing                   -> putStrLn $ "Day or part not implemented: " ++ (show s)
  where
    run :: String -> (String -> a) -> (a -> b) -> IO b
    run file parse solve =  fmap (solve . parse) (readFile file)

main :: IO ()
main = do
  args <- getArgs
  if null args
      then mapM_ runDay $ Map.keys days
      else runDay args


