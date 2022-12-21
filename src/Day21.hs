module Day21 (solve1, solve2, parseInput, Input, Output) where

import Data.Attoparsec.Text (decimal, parseOnly, choice, sepBy, string, char, endOfLine, anyChar, takeWhile1, signed)
import Data.Text (Text, pack, unpack)
import Data.Either (fromRight)
import Data.Char (isLetter)

import qualified Data.Map as M
import qualified Data.Attoparsec.Text as P (takeWhile)

data Expr = Expr Expr Char (Int -> Int -> Int) Expr | MonkeyExpr Text | ConstExpr Int

instance Show Expr where
  show (Expr l s _ r)   = "(" ++ show l ++ [' ', s, ' '] ++ show r ++ ")"
  show (ConstExpr s)  = show s
  show (MonkeyExpr n) = show n

type Input = M.Map Text Expr
type Output = Int

solve1 :: Input -> Output
solve1 input = resolve input (input M.! pack "root")

solve2 :: Input -> Output
solve2 input
  | isConstExpr rL = solveExpr rR (value rL)
  | isConstExpr rR = solveExpr rL (value rR)
  where
    Expr rL _ _ rR = reduce input (Expr mL '=' (\_ _ -> 0) mR)
    Expr mL _ _ mR = input M.! pack "root"

resolve :: Input -> Expr -> Int
resolve input (ConstExpr value)    = value
resolve input (Expr left _ op right) = resolve input left `op` resolve input right
resolve input (MonkeyExpr name)    = resolve input $ input M.! name

reduce :: Input -> Expr -> Expr
reduce input (ConstExpr value)    = ConstExpr value
reduce input (Expr left s op right)
  | canResolve = ConstExpr $ value lResolved `op` value rResolved
  | otherwise  = Expr lResolved s op rResolved
  where
    lResolved  = reduce input left
    rResolved  = reduce input right
    canResolve = isConstExpr lResolved && isConstExpr rResolved
reduce input (MonkeyExpr name)    = case unpack name of
  "humn" -> MonkeyExpr name
  _      -> reduce input $ input M.! name

solveExpr :: Expr -> Int -> Int
solveExpr (ConstExpr value) _  = value
solveExpr (MonkeyExpr _) final = final
solveExpr (Expr (ConstExpr value) '+' _ r) final = solveExpr r $ final - value
solveExpr (Expr l '+' _ (ConstExpr value)) final = solveExpr l $ final - value
solveExpr (Expr (ConstExpr value) '-' _ r) final = solveExpr r $ (final - value) * (-1)
solveExpr (Expr l '-' _ (ConstExpr value)) final = solveExpr l $ final + value
solveExpr (Expr (ConstExpr value) '*' _ r) final = solveExpr r $ final `div` value
solveExpr (Expr l '*' _ (ConstExpr value)) final = solveExpr l $ final `div` value
solveExpr (Expr (ConstExpr value) '/' _ r) final = solveExpr r $ value `div` final
solveExpr (Expr l '/' _ (ConstExpr value)) final = solveExpr l $ final * value

isConstExpr :: Expr -> Bool
isConstExpr (ConstExpr _) = True
isConstExpr _             = False

value :: Expr -> Int
value (ConstExpr v) = v

parseInput :: String -> Input
parseInput = M.fromList . fromRight (error "Unable to parse input") . parseOnly (line `sepBy` endOfLine) . pack
 where
  line = do
    monkey <- P.takeWhile (/= ':')
    _      <- string (pack ": ")
    calc   <- choice [calculation, constExpr]
    return (monkey, calc)

  calculation = do
    left   <- choice [monkeyExpr, constExpr]
    _      <- char ' '
    (s, op)<- operation
    _      <- char ' '
    right  <- choice [monkeyExpr, constExpr]
    return $ Expr left s op right

  monkeyExpr = MonkeyExpr <$> takeWhile1 isLetter
  constExpr = ConstExpr <$> signed decimal

  operation = do
    op <- anyChar
    return (op, case op of
      '+' -> (+)
      '-' -> (-)
      '/' -> div
      '*' -> (*))