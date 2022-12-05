module Day05Spec (spec) where

import Test.Hspec
import Data.Array

import Day05

exampleInput :: Input
exampleInput = (stacks, moves)
  where
    stacks = array (1,3)
      [ (1, [Crate 'N', Crate 'Z'])
      , (2, [Crate 'D', Crate 'C', Crate 'M'])
      , (3, [Crate 'P'])
      ]
    moves = 
      [ Move 1 2 1
      , Move 3 1 3
      , Move 2 2 1
      , Move 1 1 2
      ]


spec :: Spec
spec = do
  it "solve part 1 on example input" $ do
    solve1 exampleInput `shouldBe` "CMZ"

  it "solve part 2 on example input" $ do
     solve2 exampleInput `shouldBe` "MCD"