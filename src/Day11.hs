module Day11 (solve1, solve2, parseInput, Input, Output, OpValue(..), Operation(..), Monkey(..)) where

import Data.Attoparsec.Text (Parser, char, choice, decimal, endOfLine, parseOnly, sepBy, string, take)
import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.List (sortOn)
import Data.Ord

data OpValue = OldValue | ConstValue Int deriving (Show)

type Items = [Int]
type DivBy = Int
type TrueMonkey  = Int
type FalseMonkey = Int
type InspectedItems = Int
data Operation = Operation (Int -> Int -> Int) OpValue

type State = (Int, Int)

instance Show Operation where
  show (Operation _ v) = "Operation old <op> " ++ show v

data Monkey = Monkey Items InspectedItems Operation DivBy TrueMonkey FalseMonkey deriving (Show)

type Input = V.Vector Monkey
type Output = Int

solve1 :: Input -> Output
solve1 monke = product . Prelude.take 2 . sortOn Down . V.toList . V.map inspected $ simulateRounds (3, worryLevelBound monke) 20 monke

solve2 :: Input -> Output
solve2 monke = product . Prelude.take 2 . sortOn Down . V.toList . V.map inspected $ simulateRounds (1, worryLevelBound monke) 10000 monke

simulateRounds :: State -> Int -> V.Vector Monkey -> V.Vector Monkey
simulateRounds state round monkeys
  | round == 0  = monkeys
  | otherwise   = simulateRounds state (round - 1) $ simulateRound state 0 monkeys

simulateRound :: State -> Int -> V.Vector Monkey -> V.Vector Monkey
simulateRound state tMonke monkeys
  | tMonke == V.length monkeys = monkeys
  | otherwise                  = simulateRound state (tMonke + 1) $ updateItems tMonke (throwItems state $ monkeys V.! tMonke) monkeys

updateItems :: Int -> M.Map Int Items -> V.Vector Monkey -> V.Vector Monkey
updateItems source items monkeys = monkeys V.// updates
  where
    updates = (source, emptyMonke $ monkeys V.! source) : map updateIdx (M.assocs items)
    updateIdx (idx, is) =  (idx, monkeys V.! idx `catchItems` is)

throwItems :: State -> Monkey -> M.Map Int Items
throwItems (divValue, maxBound) (Monkey is _ (Operation op lV) divBy tM fM) = foldl processItem M.empty is
  where
    processItem out item = M.insertWith (flip (++)) (targetMonke item) [worryLevel item] out
    worryLevel item = op item (opValue item lV) `div` divValue `mod` maxBound
    targetMonke item
      | worryLevel item `mod` divBy == 0 = tM
      | otherwise                        = fM

opValue :: Int -> OpValue -> Int
opValue old OldValue     = old
opValue _ (ConstValue v) = v

catchItems :: Monkey -> Items -> Monkey
catchItems (Monkey currentIs i a b c d) newIs = Monkey (currentIs ++ newIs) i a b c d

emptyMonke :: Monkey -> Monkey
emptyMonke (Monkey is i a b c d) = Monkey [] (i + length is) a b c d

inspected :: Monkey -> Int
inspected (Monkey _ i _ _ _ _) = i

divByValue :: Monkey -> Int
divByValue (Monkey _ _ _ value _ _) = value

worryLevelBound :: V.Vector Monkey -> Int
worryLevelBound = V.foldl (\counter monkey -> counter * divByValue monkey) 1


parseInput :: String -> Input
parseInput = V.fromList . fromRight (error "Unable to parse input") . parseOnly parseInput' . T.pack
  where
    parseInput' :: Parser [Monkey]
    parseInput' = monkey `sepBy` emptyLine

    emptyLine = do
      _ <- endOfLine
      endOfLine

    monkey = do
      _ <- string $ T.pack "Monkey "
      _ <- decimal 
      _ <- char ':'
      _ <- endOfLine
      i <- items
      o <- operation
      d <- divBy
      t <- throwTo "true"
      _ <- endOfLine
      f <- throwTo "false"
      return $ Monkey i 0 o d t f

    items = do
      _  <- string $ T.pack "  Starting items: "
      is <- decimal `sepBy` string (T.pack ", ")
      _  <- endOfLine
      return is

    divBy = do
      _ <- string $ T.pack "  Test: divisible by "
      v <- decimal
      _ <- endOfLine
      return v

    throwTo branch = do
      _ <- string $ T.pack ("    If " ++ branch ++ ": throw to monkey ")
      decimal

    operation = do
        _    <- string $ T.pack "  Operation: new = old "
        op   <- Data.Attoparsec.Text.take 1
        _    <- char ' '
        sym  <- choice [oldValue, constValue]
        _    <- endOfLine
        return $ case T.head op of
            '+' -> Operation (+) sym
            '-' -> Operation (-) sym
            '*' -> Operation (*) sym

    oldValue = do
        _ <- string $ T.pack "old"
        return OldValue
    
    constValue = ConstValue <$> decimal
