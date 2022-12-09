module Day09Spec (spec) where

import qualified Data.Matrix as M

import Test.Hspec
import Day09

exampleInput :: [Movement]
exampleInput = 
  [ R 4
  , U 4
  , L 3
  , D 1
  , R 4
  , D 1
  , L 5
  , R 2
  ]

exampleInput2 = 
  [ R 5
  , U 8
  , L 8
  , D 3
  , R 17
  , D 10
  , L 25
  , U 20
  ]

spec :: Spec
spec = do
  it "solve part 1 on example input" $ do
    solve1 exampleInput `shouldBe` 13

  it "solve part 2 on example input" $ do
     solve2 exampleInput `shouldBe` 1
     solve2 exampleInput2 `shouldBe` 36
