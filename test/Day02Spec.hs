module Day02Spec (spec) where

import Test.Hspec
import Day02

exampleInput :: Input
exampleInput =
  [ ["A", "Y"]
  , ["B", "X"]
  , ["C", "Z"]
  ]

spec :: Spec
spec = do
  it "solve part 1 on example input" $ do
    solve1 exampleInput `shouldBe` 15

  it "solve part 2 on example input" $ do
     solve2 exampleInput `shouldBe` 12