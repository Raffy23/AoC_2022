module Day07Spec (spec) where

import Test.Hspec
import Data.Text
import Day07

exampleInput :: Input
exampleInput = Dir "/" 48381165 [
    Dir "a" 94853 [
        Dir "e" 584 [
            File (pack "i") 584
        ],
        File (pack "f") 29116,
        File (pack "g") 2557,
        File (pack "h.lst") 62596
    ],
    File (pack "b.txt") 14848514,
    File (pack "c.dat") 8504156,
    Dir "d" 24933642 [
        File (pack "j") 4060174,
        File (pack "d.log") 8033020,
        File (pack "d.ext") 5626152,
        File (pack "k") 7214296
    ]]

spec :: Spec
spec = do
  it "solve part 1 on example input" $ do
    solve1 exampleInput `shouldBe` 95437

  it "solve part 2 on example input" $ do
     solve2 exampleInput `shouldBe` 24933642