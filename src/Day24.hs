module Day24 (solve1, solve2, parseInput, Input, Output) where

import Data.Attoparsec.Text (decimal, Parser, parseOnly, many', endOfLine, sepBy, choice, char)
import Data.Text (pack)
import Data.Either (fromRight)
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Data.Ord

import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Set as SE

import Debug.Trace

type Point = (Int, Int)
type Vally = M.Map Point [Char]

type Input = Vally
type Output = Int

type Cache = SE.Set (Point, Vally)

solve1 :: Input -> Output
solve1 vally = fst $ findPath vally (exit vally) 0 (SE.fromList [entrance vally]) SE.empty

solve2 :: Input -> Output
solve2 vally = trip1 + trip2 + trip3
  where
    (trip1, vally' ) = findPath vally   (exit vally) 0 (SE.fromList [entrance vally]) SE.empty
    (trip2, vally'') = findPath vally'  (entrance vally) 1 (SE.fromList [exit vally]) SE.empty
    (trip3, _      ) = findPath vally'' (exit vally) 1 (SE.fromList [entrance vally]) SE.empty

findPath :: Vally -> Point -> Int -> SE.Set Point -> Cache -> (Int, Vally)
findPath vally target time points cache
  | SE.member target points = (time, vally')
  | otherwise               = findPath vally' target (time + 1) points' cache'
  where
    (maxR, _)         = fst $ M.findMax vally
    (vally', points') = step (vally, points) (maxR + 1) cache
    cache'            = foldl (\c point -> SE.insert (point, vally') c) cache points'

step :: (Vally, SE.Set Point) -> Int -> Cache -> (Vally, SE.Set Point)
step (vally, points) border cache = (vally', points')
  where
    vally'   = moveBlizzard vally
    points'  = foldl (\s point -> SE.union s $ SE.fromList (moveElf vally' border cache point)) SE.empty points

moveElf :: Vally -> Int -> Cache -> Point -> [Point]
moveElf vally border cache (r,c) = filter shouldMove [ (r, c+1), (r+1,c), (r-1,c), (r,c-1), (r,c) ]
  where
    shouldMove point@(r, c) = M.notMember point vally && SE.notMember (point, vally) cache && r < border

isElfAlive :: Vally -> Point -> Bool
isElfAlive vally point = M.notMember point vally

moveBlizzard :: Vally -> Vally
moveBlizzard vally = foldl (\vally (point, bs) -> update vally $ move' point bs) M.empty (M.assocs vally)
  where
    update :: Vally -> [(Point, Char)] -> Vally
    update vally bs = foldl (\v (p, b) -> M.alter (merge b) p v) vally bs

    move' :: Point -> [Char] -> [(Point, Char)]
    move' p bs = map (move p) bs

    move :: Point -> Char -> (Point, Char)
    move (r, c) '^' = warp ((r - 1, c), '^')
    move (r, c) '<' = warp ((r, c - 1), '<')
    move (r, c) '>' = warp ((r, c + 1), '>')
    move (r, c) 'v' = warp ((r + 1, c), 'v')
    move (r, c) '#' = ((r, c), '#')

    (maxR, maxC) = fst $ M.findMax vally

    warp :: (Point, Char) -> (Point, Char)
    warp ((0, c), b) = ((maxR - 1, c), b)
    warp ((r, 0), b) = ((r, maxC - 1), b)
    warp p@((r, c), b)
      | r == maxR = ((1, c), b)
      | c == maxC = ((r, 1), b)
      | otherwise = p

    merge :: Char -> Maybe [Char] -> Maybe [Char]
    merge b (Just bs) = Just $ b : bs
    merge b Nothing   = Just [b]

entrance :: Vally -> Point
entrance vally = fromJust $ find (`M.notMember` vally) [ (0, c) | c <- [0..] ]

exit :: Vally -> Point
exit vally = fromJust $ find (`M.notMember` vally) [ (fst $ fst $ M.findMax vally, c) | c <- [0..] ]

parseInput :: String -> Input
parseInput str = M.insert (eR - 1, eC) ['#'] vally
 where
   vally    = fromRight (error "Unable to parse input") . parseOnly parseInput' $ pack str
   (eR, eC) = entrance vally
   parseInput' = do
     lines <- line `sepBy` endOfLine
     return $ M.fromList $ filter (\(_, c) -> head c /= '.') $ concatMap (\(r,rs) -> map (\(c, v) -> ((r,c),[v])) rs) (zip [0..] lines)

   line = do
     lines <- many' $ choice [char '#', char '.', char '>', char '^', char 'v', char '<']
     return $ zip [0..] lines

showVally :: Vally -> [String]
showVally vally = [ row r | r <- [0 .. maxR] ]
  where
    (maxR, maxC) = fst $ M.findMax vally

    row :: Int -> String
    row r = [ tile (r, c) | c <- [0 .. (maxC + 1)] ]

    tile :: Point -> Char
    tile p = case M.lookup p vally of
        Just [c]  -> c
        Just bs   -> head . show $ length bs
        Nothing   -> '.'