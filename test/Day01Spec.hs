module Day01Spec (spec) where

import Test.Hspec
import Day01

exampleInput :: Input
exampleInput =
  [ [1000, 2000, 3000]
  , [4000]
  , [5000, 6000]
  , [7000, 8000, 9000]
  , [10000]
  ]

spec :: Spec
spec = do
  it "solve part 1 on example input" $ do
    solve1 exampleInput `shouldBe` 24000

  it "solve part 2 on example input" $ do
     solve2 exampleInput `shouldBe` 45000