module Day06Spec (spec) where

import Test.Hspec
import Day06

examples :: [(Input, Output, Output)]
examples =
  [ ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7, 19)
  , ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5, 23)
  , ("nppdvjthqldpwncqszvftbrmjlhg", 6, 23)
  , ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10, 29)
  , ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11, 26)
  ]

spec :: Spec
spec = do
  it "solve part 1 on example input" $ do
    mapM_ (\(input, output, _) -> solve1 input `shouldBe` output) examples

  it "solve part 2 on example input" $ do
     mapM_ (\(input, _, output) -> solve2 input `shouldBe` output) examples