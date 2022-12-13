module Day13 (solve1, solve2, parseInput, Input, Output) where

import Data.Attoparsec.Text (decimal, parseOnly, choice, sepBy, char, endOfLine)
import Data.Text (pack)
import Data.Either (fromRight)
import Data.List (sort, findIndices)

data List = List [List] | Elem Int deriving (Eq)
type Packet = (List, List)

type Input = [Packet]
type Output = Int

solve1 :: Input -> Output
solve1 = foldl count 0 . zip [1..] . map (uncurry (<=))
  where
    count c (p, True)  = c + p
    count c (_, False) = c

solve2 :: Input -> Int
solve2 = product . map (+ 1) . findIndices isDivider . sort . (++) divider . concatMap (\(l,r) -> [l, r])
  where
    isDivider a = a `elem` divider
    divider =
      [ List [List [Elem 2]]
      , List [List [Elem 6]]
      ]

instance Ord List where
  compare (List []) (List []) = EQ
  compare (List []) (List  _) = LT
  compare (List  _) (List []) = GT
  compare (List (l:ls)) (List (r:rs)) = case compare l r of
    EQ  -> compare ls rs
    out -> out
  compare (Elem l) (Elem r) = compare l r
  compare (Elem l) (List rs) = compare (List [Elem l]) (List rs)
  compare (List ls) (Elem r) = compare (List ls) (List [Elem r])

instance Show List where
  show (Elem e) = show e
  show (List xs) = show xs

parseInput :: String -> Input
parseInput = fromRight (error "Unable to parse input") . parseOnly (packet `sepBy` emptyLine) . pack
  where
    packet = do
      l1 <- list
      _  <- endOfLine
      l2 <- list
      return (l1, l2)

    list = do
      _   <- char '['
      els <- choice [list, element] `sepBy` char ','
      _   <- char ']'
      return $ List els

    element = Elem <$> decimal

    emptyLine = do
      _ <- endOfLine
      endOfLine

