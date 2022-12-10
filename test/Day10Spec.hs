module Day10Spec (spec) where

import qualified Data.Matrix as M

import Test.Hspec
import Day10

exampleInput:: [Instruction]
exampleInput = 
  [AddX 15
  ,AddX (-11)
  ,AddX 6
  ,AddX (-3)
  ,AddX 5
  ,AddX (-1)
  ,AddX (-8)
  ,AddX 13
  ,AddX 4
  ,Noop
  ,AddX (-1)
  ,AddX 5
  ,AddX (-1)
  ,AddX 5
  ,AddX (-1)
  ,AddX 5
  ,AddX (-1)
  ,AddX 5
  ,AddX (-1)
  ,AddX (-35)
  ,AddX 1
  ,AddX 24
  ,AddX (-19)
  ,AddX 1
  ,AddX 16
  ,AddX (-11)
  ,Noop
  ,Noop
  ,AddX 21
  ,AddX (-15)
  ,Noop
  ,Noop
  ,AddX (-3)
  ,AddX 9
  ,AddX 1
  ,AddX (-3)
  ,AddX 8
  ,AddX 1
  ,AddX 5
  ,Noop
  ,Noop
  ,Noop
  ,Noop
  ,Noop
  ,AddX (-36)
  ,Noop
  ,AddX 1
  ,AddX 7
  ,Noop
  ,Noop
  ,Noop
  ,AddX 2
  ,AddX 6
  ,Noop
  ,Noop
  ,Noop
  ,Noop
  ,Noop
  ,AddX 1
  ,Noop
  ,Noop
  ,AddX 7
  ,AddX 1
  ,Noop
  ,AddX (-13)
  ,AddX 13
  ,AddX 7
  ,Noop
  ,AddX 1
  ,AddX (-33)
  ,Noop
  ,Noop
  ,Noop
  ,AddX 2
  ,Noop
  ,Noop
  ,Noop
  ,AddX 8
  ,Noop
  ,AddX (-1)
  ,AddX 2
  ,AddX 1
  ,Noop
  ,AddX 17
  ,AddX (-9)
  ,AddX 1
  ,AddX 1
  ,AddX (-3)
  ,AddX 11
  ,Noop
  ,Noop
  ,AddX 1
  ,Noop
  ,AddX 1
  ,Noop
  ,Noop
  ,AddX (-13)
  ,AddX (-19)
  ,AddX 1
  ,AddX 3
  ,AddX 26
  ,AddX (-30)
  ,AddX 12
  ,AddX (-1)
  ,AddX 3
  ,AddX 1
  ,Noop
  ,Noop
  ,Noop
  ,AddX (-9)
  ,AddX 18
  ,AddX 1
  ,AddX 2
  ,Noop
  ,Noop
  ,AddX 9
  ,Noop
  ,Noop
  ,Noop
  ,AddX (-1)
  ,AddX 2
  ,AddX (-37)
  ,AddX 1
  ,AddX 3
  ,Noop
  ,AddX 15
  ,AddX (-21)
  ,AddX 22
  ,AddX (-6)
  ,AddX 1
  ,Noop
  ,AddX 2
  ,AddX 1
  ,Noop
  ,AddX (-10)
  ,Noop
  ,Noop
  ,AddX 20
  ,AddX 1
  ,AddX 2
  ,AddX 2
  ,AddX (-6)
  ,AddX (-11)
  ,Noop
  ,Noop
  ,Noop
  ]

-- NOTE: Last character can be ignored, not in final output 
exampleOutput2 = 
  [ "██  ██  ██  ██  ██  ██  ██  ██  ██  ██  "
  , "███   ███   ███   ███   ███   ███   ███ "
  , "████    ████    ████    ████    ████    "
  , "█████     █████     █████     █████     "
  , "██████      ██████      ██████      ████"
  , "███████       ███████       ███████     "
  , " "
  ]

spec :: Spec
spec = do
  it "solve part 1 on example input" $ do
    solve1 exampleInput `shouldBe` 13140

  it "solve part 2 on example input" $ do
     solve2 exampleInput `shouldBe` exampleOutput2
