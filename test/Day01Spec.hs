module Day01Spec (spec) where

import Test.Hspec
import Day01

example_input :: Input
example_input =
  [ [1000, 2000, 3000]
  , [4000]
  , [5000, 6000]
  , [7000, 8000, 9000]
  , [10000]
  ]

spec :: Spec
spec = do
  describe "Day01" $ do
    it "solve part 1 on example input" $ do
      solve1 example_input `shouldBe` 24000

    it "solve part 2 on example input" $ do
       solve2 example_input `shouldBe` 45000