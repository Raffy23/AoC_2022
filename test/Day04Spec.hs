module Day04Spec (spec) where

import Test.Hspec
import Day04

exampleInput :: Input
exampleInput =
  [ (Range 2 4, Range 6 8)
  , (Range 2 3, Range 4 5)
  , (Range 5 7, Range 7 9)
  , (Range 2 8, Range 3 7)
  , (Range 6 6, Range 4 6)
  , (Range 2 6, Range 4 8)
  ]

spec :: Spec
spec = do
  it "solve part 1 on example input" $ do
    solve1 exampleInput `shouldBe` 2

  it "solve part 2 on example input" $ do
     solve2 exampleInput `shouldBe` 4