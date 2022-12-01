module Main (main) where

import Lib
import qualified Day01 as Day01
import System.Environment (getArgs)
import Data.Map

days = fromList [
  ("01.1", (Day01.parseInput, Day01.solve1, "input/Day01.part1.txt"))
]

main :: IO ()
main = do
  args <- getArgs
  someFunc

runDay :: String -> (String -> a) -> (a -> b) -> IO b
runDay file parse solve =  fmap (solve . parse) (readFile file)
