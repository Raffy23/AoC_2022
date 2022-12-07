module Day07 (solve1, solve2, parseInput, Input, Output) where

import Data.Attoparsec.Text (decimal, Parser, char, string, parseOnly, isEndOfLine, many', choice, endOfLine, takeTill)
import Data.Text (Text, pack, unpack, snoc)
import Data.Either (fromRight)

data TermLine = Cd String | Ls | F Int Text | D Text deriving (Show, Eq)
data FileTree = Dir String Int [FileTree] | File Text Int deriving (Show)

type Input = FileTree
type Output = Int

solve1 :: Input -> Output
solve1 tree = sum . filter (<= 100000) $ flatten tree

solve2 :: Input -> Output
solve2 tree = minimum . filter (>= target) $ flatten tree
  where
    Dir _ rootSize _ = tree
    target = 30000000 - (70000000 - rootSize)

parseInput :: String -> FileTree
parseInput str = head . fst . parseFileTree . fromRight (error "Unable to parse input") $ parseOnly parseTermLines input
  where
    input = snoc (pack str) '\n'

parseFileTree :: [TermLine] -> ([FileTree], [TermLine])
parseFileTree [] = ([], [])
parseFileTree (Cd ".." : tls) = ([], tls)
parseFileTree (Cd dir : Ls : tls) = (currentDir : childDirs, parentRest)
  where
    currentDir = Dir dir (sizeCurrentFile + sizeSubDirFiles) $ currentFiles ++ subDirFiles
    currentFiles = map toFileTree $ filter isFile $ takeWhile isListing tls
    (subDirFiles, rest) = parseFileTree $ dropWhile isListing tls
    (childDirs, parentRest) = parseFileTree rest

    sizeCurrentFile = sum $ map size currentFiles
    sizeSubDirFiles = sum $ map size subDirFiles

parseTermLines :: Parser [TermLine]
parseTermLines = do
  many' $ choice [cd, ls, file, dir]
  where
    cd = do
      _    <- string $ pack "$ cd "
      name <- takeTill isEndOfLine
      _    <- endOfLine
      return $ Cd (unpack name)
    ls = do
      _ <- string $ pack "$ ls"
      _ <- endOfLine
      return Ls
    file = do
      size <- decimal
      _    <- char ' '
      name <- takeTill isEndOfLine
      _    <- endOfLine
      return $ F size name
    dir = do
      _    <- string $ pack "dir "
      name <- takeTill isEndOfLine
      _    <- endOfLine
      return $ D name

isFile :: TermLine -> Bool
isFile (F _ _) = True
isFile _       = False

isListing :: TermLine -> Bool
isListing (F _ _) = True
isListing (D _)   = True
isListing _       = False

toFileTree :: TermLine -> FileTree
toFileTree (F size name) = File name size

size :: FileTree -> Int
size (Dir _ s _) = s
size (File _ s)  = s

flatten :: FileTree -> [Int]
flatten (Dir _ size children) = size : concatMap flatten children
flatten (File _ _) = []