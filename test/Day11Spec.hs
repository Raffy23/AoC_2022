module Day11Spec (spec) where

import qualified Data.Vector as V

import Test.Hspec
import Day11

exampleInput :: V.Vector Monkey
exampleInput = V.fromList
  [ Monkey [79,98      ] 0 (Operation (*) (ConstValue 19)) 23 2 3
  , Monkey [54,65,75,74] 0 (Operation (+) (ConstValue 6 )) 19 2 0
  , Monkey [79,60,97   ] 0 (Operation (*) (OldValue     )) 13 1 3
  , Monkey [74         ] 0 (Operation (+) (ConstValue 3 )) 17 0 1
  ]

spec :: Spec
spec = do
  it "solve part 1 on example input" $ do
    solve1 exampleInput `shouldBe` 10605

  it "solve part 2 on example input" $ do
     solve2 exampleInput `shouldBe` 2713310158

