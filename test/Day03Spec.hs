module Day03Spec (spec) where

import Test.Hspec
import Day03

exampleInput :: Input
exampleInput =
  [ "vJrwpWtwJgWrhcsFMMfFFhFp"
  , "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
  , "PmmdzqPrVvPwwTWBwg"
  , "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
  , "ttgJtRGJQctTZtZT"
  , "CrZsJsPPZsGzwwsLwLmpwMDw"
  ]

spec :: Spec
spec = do
  it "solve part 1 on example input" $ do
    solve1 exampleInput `shouldBe` 157

  it "solve part 2 on example input" $ do
     solve2 exampleInput `shouldBe` 70