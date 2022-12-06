module Main (main) where

import System.Environment (getArgs)

import qualified Data.Map as Map

import Common (file)
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06

days :: Map.Map [String] (IO String)
days =
  Map.fromList
    [ (["01", "1"], run (file "01" "1") Day01.parseInput Day01.solve1 show)
    , (["01", "2"], run (file "01" "2") Day01.parseInput Day01.solve2 show)
    , (["02", "1"], run (file "02" "1") Day02.parseInput Day02.solve1 show)
    , (["02", "2"], run (file "02" "2") Day02.parseInput Day02.solve2 show)
    , (["03", "1"], run (file "03" "1") Day03.parseInput Day03.solve1 show)
    , (["03", "2"], run (file "03" "2") Day03.parseInput Day03.solve2 show)
    , (["04", "1"], run (file "04" "1") Day04.parseInput Day04.solve1 show)
    , (["04", "2"], run (file "04" "1") Day04.parseInput Day04.solve2 show)
    , (["05", "1"], run (file "05" "1") Day05.parseInput Day05.solve1 id  )
    , (["05", "2"], run (file "05" "1") Day05.parseInput Day05.solve2 id  )
    , (["06", "1"], run (file "06" "1") Day06.parseInput Day06.solve1 show)
    , (["06", "2"], run (file "06" "1") Day06.parseInput Day06.solve2 show)
    ]

runDay :: [String] -> IO ()
runDay s = case Map.lookup s days of
  Just f   -> do 
    output <- f
    _      <- putStrLn $ "---- Day " ++ show s ++ " ----"
    _      <- putStrLn output
    return ()
  Nothing  -> putStrLn $ "Day or part not implemented: " ++ show s

run :: String -> (String -> a) -> (a -> b) -> (b -> String) -> IO String
run file parse solve print =  fmap (print . solve . parse) (readFile file)

main :: IO ()
main = do
  args <- getArgs
  if null args
      then mapM_ runDay $ Map.keys days
      else runDay args


