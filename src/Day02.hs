{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day02 (solve1, solve2, parseInput, Input, Output) where

type Input = [[String]]
type Output = Int

data Pick =
    Rock
  | Paper
  | Scissors
  deriving (Bounded, Eq, Show)

data GameOutcome =
     Won
   | Loss
   | Draw
   deriving (Bounded, Eq, Show)

solve1 :: Input -> Output
solve1 = sum . map score1
  where
    score1 :: [String] -> Int
    score1 [opponent, self] = winningPoints gameOutcome + points (toPick self)
      where
        gameOutcome   = winning (toPick self) (toPick opponent)

solve2 :: Input -> Output
solve2 = sum . map score2
  where
    score2 :: [String] -> Int
    score2 [opponent, outcome] = winningPoints gameOutcome + points (self gameOutcome opponentPick)
      where
        opponentPick  = toPick opponent
        gameOutcome   = toGameOutcome outcome
        self Won Rock = Paper
        self Won Scissors = Rock
        self Won Paper = Scissors
        self Draw a = a
        self Loss Rock = Scissors
        self Loss Scissors = Paper
        self Loss Paper = Rock

parseInput :: String -> Input
parseInput = map words . lines

toPick :: String -> Pick
toPick "A" = Rock
toPick "B" = Paper
toPick "C" = Scissors
toPick "X" = Rock
toPick "Y" = Paper
toPick "Z" = Scissors

toGameOutcome :: String -> GameOutcome
toGameOutcome "X" = Loss
toGameOutcome "Y" = Draw
toGameOutcome "Z" = Won

points :: Pick -> Int
points Rock     = 1
points Paper    = 2
points Scissors = 3

winning :: Pick -> Pick -> GameOutcome
winning Rock Scissors  = Won
winning Scissors Paper = Won
winning Paper Rock     = Won
winning a b = if a == b
  then Draw
  else Loss

winningPoints :: Num a => GameOutcome -> a
winningPoints Won  = 6
winningPoints Draw = 3
winningPoints Loss = 0

