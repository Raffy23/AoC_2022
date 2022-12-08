module Day08Spec (spec) where

import qualified Data.Matrix as M

import Test.Hspec
import Day08

exampleInput :: M.Matrix Int
exampleInput = M.fromLists [ 
  [3,0,3,7,3],
  [2,5,5,1,2],
  [6,5,3,3,2],
  [3,3,5,4,9],
  [3,5,3,9,0]]

spec :: Spec
spec = do
  it "solve part 1 on example input" $ do
    solve1 exampleInput `shouldBe` 21

  it "solve part 2 on example input" $ do
     solve2 exampleInput `shouldBe` 8

