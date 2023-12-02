{-# LANGUAGE TupleSections #-}

module Main (main) where

import Criterion.Main
import qualified GHC.IO.Encoding as E
import qualified Data.Map as M

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
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25

inputs :: [(String, String)]
inputs = 
  [ ("01", "1")
  , ("02", "1")
  , ("03", "1")
  , ("04", "1")
  , ("05", "1")
  , ("06", "1")
  , ("07", "1")
  , ("08", "1")
  , ("09", "1")
  , ("10", "1")
  , ("11", "1")
  , ("12", "1")
  , ("13", "1")
  , ("14", "1")
  , ("15", "1")
  , ("16", "1")
  , ("17", "1")
  , ("18", "1")
  , ("19", "1")
  , ("20", "1")
  , ("21", "1")
  , ("22", "1")
  , ("23", "1")
  , ("24", "1")
  , ("25", "1")
  ]

setupEnv :: IO (M.Map (String, String) String)
setupEnv = do
  files <- mapM (\ (day, part) -> fmap ((day, part),) $ readFile $ file day part) inputs
  return $ M.fromList files

main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  defaultMain 
    [ env setupEnv $ \ inputs -> bgroup "main" 
      [ bgroup "Day01" 
        [ bench "Part 1" $ whnf (Day01.solve1 . Day01.parseInput) $ inputs M.! ("01", "1")
        , bench "Part 2" $ whnf (Day01.solve2 . Day01.parseInput) $ inputs M.! ("01", "1")
        ]
      , bgroup "Day02" 
        [ bench "Part 1" $ whnf (Day02.solve1 . Day02.parseInput) $ inputs M.! ("02", "1")
        , bench "Part 2" $ whnf (Day02.solve2 . Day02.parseInput) $ inputs M.! ("02", "1")
        ]
      , bgroup "Day03" 
        [ bench "Part 1" $ whnf (Day03.solve1 . Day03.parseInput) $ inputs M.! ("03", "1")
        , bench "Part 2" $ whnf (Day03.solve2 . Day03.parseInput) $ inputs M.! ("03", "1")
        ] 
      , bgroup "Day04" 
        [ bench "Part 1" $ whnf (Day04.solve1 . Day04.parseInput) $ inputs M.! ("04", "1")
        , bench "Part 2" $ whnf (Day04.solve2 . Day04.parseInput) $ inputs M.! ("04", "1")
        ]
      , bgroup "Day05" 
        [ bench "Part 1" $ whnf (Day05.solve1 . Day05.parseInput) $ inputs M.! ("05", "1")
        , bench "Part 2" $ whnf (Day05.solve2 . Day05.parseInput) $ inputs M.! ("05", "1")
        ]
      , bgroup "Day06" 
        [ bench "Part 1" $ whnf (Day06.solve1 . Day06.parseInput) $ inputs M.! ("06", "1")
        , bench "Part 2" $ whnf (Day06.solve2 . Day06.parseInput) $ inputs M.! ("06", "1")
        ]
      , bgroup "Day07" 
        [ bench "Part 1" $ whnf (Day07.solve1 . Day07.parseInput) $ inputs M.! ("07", "1")
        , bench "Part 2" $ whnf (Day07.solve2 . Day07.parseInput) $ inputs M.! ("07", "1")
        ]
      , bgroup "Day08" 
        [ bench "Part 1" $ whnf (Day08.solve1 . Day08.parseInput) $ inputs M.! ("08", "1")
        , bench "Part 2" $ whnf (Day08.solve2 . Day08.parseInput) $ inputs M.! ("08", "1")
        ]
      , bgroup "Day09" 
        [ bench "Part 1 " $ whnf (Day09.solve1 . Day09.parseInput) $ inputs M.! ("09", "1")
--        , bench "Part 1a" $ whnf (Day09.solve1a. Day09.parseInput) $ inputs M.! ("09", "1")
        , bench "Part 2 " $ whnf (Day09.solve2 . Day09.parseInput) $ inputs M.! ("09", "1")
        ]
      , bgroup "Day10" 
        [ bench "Part 1" $ whnf (Day10.solve1 . Day10.parseInput) $ inputs M.! ("10", "1")
        , bench "Part 2" $ whnf (Day10.solve2 . Day10.parseInput) $ inputs M.! ("10", "1")
        ]
      , bgroup "Day11" 
        [ bench "Part 1" $ whnf (Day11.solve1 . Day11.parseInput) $ inputs M.! ("11", "1")
        , bench "Part 2" $ whnf (Day11.solve2 . Day11.parseInput) $ inputs M.! ("11", "1")
        ]
      , bgroup "Day12" 
        [ bench "Part 1" $ whnf (Day12.solve1 . Day12.parseInput) $ inputs M.! ("12", "1")
        , bench "Part 2" $ whnf (Day12.solve2 . Day12.parseInput) $ inputs M.! ("12", "1")
        ]
      , bgroup "Day13" 
        [ bench "Part 1" $ whnf (Day13.solve1 . Day13.parseInput) $ inputs M.! ("13", "1")
        , bench "Part 2" $ whnf (Day13.solve2 . Day13.parseInput) $ inputs M.! ("13", "1")
        ]
      , bgroup "Day14" 
        [ bench "Part 1" $ whnf (Day14.solve1 . Day14.parseInput) $ inputs M.! ("14", "1")
        , bench "Part 2" $ whnf (Day14.solve2 . Day14.parseInput) $ inputs M.! ("14", "1")
        ]
      , bgroup "Day15" 
        [ bench "Part 1" $ whnf (Day15.solve1 . Day15.parseInput) $ inputs M.! ("15", "1")
        , bench "Part 2" $ whnf (Day15.solve2 . Day15.parseInput) $ inputs M.! ("15", "1")
        ]
      , bgroup "Day16" 
        [ bench "Part 1" $ whnf (Day16.solve1 . Day16.parseInput) $ inputs M.! ("16", "1")
        , bench "Part 2" $ whnf (Day16.solve2 . Day16.parseInput) $ inputs M.! ("16", "1")
        ]
      , bgroup "Day17" 
        [ bench "Part 1" $ whnf (Day17.solve1 . Day17.parseInput) $ inputs M.! ("17", "1")
        , bench "Part 2" $ whnf (Day17.solve2 . Day17.parseInput) $ inputs M.! ("17", "1")
        ]
      , bgroup "Day18" 
        [ bench "Part 1" $ whnf (Day18.solve1 . Day18.parseInput) $ inputs M.! ("18", "1")
        , bench "Part 2" $ whnf (Day18.solve2 . Day18.parseInput) $ inputs M.! ("18", "1")
        ]
      , bgroup "Day19" 
        [ bench "Part 1" $ whnf (Day19.solve1 . Day19.parseInput) $ inputs M.! ("19", "1")
        , bench "Part 2" $ whnf (Day19.solve2 . Day19.parseInput) $ inputs M.! ("19", "1")
        ]
      , bgroup "Day20" 
        [ bench "Part 1" $ whnf (Day20.solve1 . Day20.parseInput) $ inputs M.! ("20", "1")
        , bench "Part 2" $ whnf (Day20.solve2 . Day20.parseInput) $ inputs M.! ("20", "1")
        ]
      , bgroup "Day21" 
        [ bench "Part 1" $ whnf (Day21.solve1 . Day21.parseInput) $ inputs M.! ("21", "1")
        , bench "Part 2" $ whnf (Day21.solve2 . Day21.parseInput) $ inputs M.! ("21", "1")
        ]
      , bgroup "Day22" 
        [ bench "Part 1" $ whnf (Day22.solve1 . Day22.parseInput) $ inputs M.! ("22", "1")
        , bench "Part 2" $ whnf (Day22.solve2 . Day22.parseInput) $ inputs M.! ("22", "1")
        ]
      , bgroup "Day23" 
        [ bench "Part 1" $ whnf (Day23.solve1 . Day23.parseInput) $ inputs M.! ("23", "1")
        , bench "Part 2" $ whnf (Day23.solve2 . Day23.parseInput) $ inputs M.! ("23", "1")
        ]
      , bgroup "Day24" 
        [ bench "Part 1" $ whnf (Day24.solve1 . Day24.parseInput) $ inputs M.! ("24", "1")
        , bench "Part 2" $ whnf (Day24.solve2 . Day24.parseInput) $ inputs M.! ("24", "1")
        ]
       , bgroup "Day25" 
        [ bench "Part 1" $ whnf (Day25.solve1 . Day25.parseInput) $ inputs M.! ("25", "1")
        ]
      ]
    ]