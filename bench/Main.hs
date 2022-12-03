{-# LANGUAGE TupleSections #-}

module Main (main) where

import Criterion.Main
import qualified GHC.IO.Encoding as E
import qualified Data.Map as M

import Common (file)
import qualified Day01
import qualified Day02
import qualified Day03

inputs :: [(String, String)]
inputs = 
  [ ("01", "1")
  , ("02", "1")
  , ("03", "1")
  ]

setupEnv :: IO (M.Map (String, String) String)
setupEnv = do
  files <- mapM (\ (day, part) -> fmap ((day, part),) $ readFile $ file day part) inputs
  return $ M.fromList files

main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  defaultMain 
    [
    -- notice the lazy pattern match here!
    env setupEnv $ \ inputs -> bgroup "main" 
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
      ] 
    ]