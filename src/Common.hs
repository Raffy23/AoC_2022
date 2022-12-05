module Common (file, transpose) where

import Text.Printf (printf)

file :: String -> String -> String
file = printf "input/Day%s.part%s.txt"

transpose :: [[a]]->[[a]]
transpose ([]:_) = []
transpose x      = map head x : transpose (map tail x)
