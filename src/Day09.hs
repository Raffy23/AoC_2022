module Day09 (solve1, solve2, parseInput, Input, Output, Movement(..)) where

import Data.Attoparsec.Text (decimal, Parser, parseOnly, sepBy, char, endOfLine, letter)
import Data.Text (pack)
import Data.Either (fromRight)
import qualified Data.Set as S

data Movement = R Int | U Int | L Int | D Int deriving (Show, Eq)
type Rope = [(Int, Int)]
type Points = S.Set (Int, Int)
type Knot = (Int, Int)

type Input = [Movement]
type Output = Int

solve1 :: Input -> Output
solve1 input = S.size positions
  where
    rope = ((0,0), (0,0), S.fromList [(0,0)])
    (_, _, positions) = foldl moveRope rope input

    moveRope :: (Knot, Knot, Points) -> Movement -> (Knot, Knot, Points)
    moveRope rope movement = foldl moveKnot rope (steps movement)

    moveKnot :: (Knot, Knot, Points) -> (Int, Int) -> (Knot, Knot, Points)
    moveKnot ((hX, hY), tail, set) (sX, sY) = (head', tail', set')
      where
        head' = (hX + sX, hY + sY)
        tail' = follow head' tail
        set'  = S.insert tail' set

-- For some reason if solve1a is used Part 2 take 20 ms instead of 4.737 ms
-- solve1a :: Input -> Output
-- solve1a = S.size . snd . foldl moveRope (replicate 2 (0,0), S.fromList [(0,0)])

solve2 :: Input -> Output
solve2 = S.size . snd . foldl moveRope (replicate 10 (0,0), S.fromList [(0,0)])

moveRope :: (Rope, Points) -> Movement -> (Rope, Points)
moveRope rope movement = foldl moveKnot rope (steps movement)

moveKnot :: (Rope, Points) -> (Int, Int) -> (Rope, Points)
moveKnot ((hX, hY):tails, set) (sX, sY) = (head':tails', set')
  where
    head'  = (hX + sX, hY + sY)
    tails' = follow' head' tails
    tail   = last tails'
    set'   = S.insert tail set

follow' :: Knot -> [Knot] -> [Knot]
follow' head [tail]       = [follow head tail]
follow' head (tail:tails) = head' : follow' head' tails
  where
    head' = follow head tail 

follow :: (Int, Int) -> (Int, Int) -> (Int, Int)
follow (hX, hY) (x, y)
  | abs (hX - x) > 1 || abs (hY - y) > 1 = (x + signum (hX - x), y + signum (hY - y)) 
  | otherwise                            = (x, y)

steps :: Movement -> [(Int, Int)]
steps (R n) = replicate n ( 0,  1)
steps (L n) = replicate n ( 0, -1)
steps (U n) = replicate n (-1,  0)
steps (D n) = replicate n ( 1,  0)

parseInput :: String -> Input
parseInput = fromRight (error "Unable to parse input") . parseOnly parseInput' . pack
  where
    parseInput' :: Parser [Movement]
    parseInput' = movement `sepBy` endOfLine
  
    movement = do
      direction <- letter
      _         <- char ' '
      case direction of
        'R' -> R <$> decimal
        'L' -> L <$> decimal
        'U' -> U <$> decimal
        'D' -> D <$> decimal
