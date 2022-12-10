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
        , bench "Part 1" $ whnf (Day10.solve1 . Day10.parseInput) $ inputs M.! ("10", "1")
        ]
      ]
    ]