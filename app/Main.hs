module Main (main) where

import System.Environment (getArgs)
import Text.Printf (printf)

import qualified Data.Map as Map

import qualified Day01
import qualified Day02
import qualified Day03

file :: String -> String -> String
file = printf "input/Day%s.part%s.txt"

days :: Map.Map [String] (IO Int)
days =
  Map.fromList
    [ (["01", "1"], run (file "01" "1") Day01.parseInput Day01.solve1)
    , (["01", "2"], run (file "01" "2") Day01.parseInput Day01.solve2)
    , (["02", "1"], run (file "02" "1") Day02.parseInput Day02.solve1)
    , (["02", "2"], run (file "02" "2") Day02.parseInput Day02.solve2)
    , (["03", "1"], run (file "03" "1") Day03.parseInput Day03.solve1)
    , (["03", "2"], run (file "03" "2") Day03.parseInput Day03.solve2)
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


