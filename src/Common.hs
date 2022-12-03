module Common (file) where

import Text.Printf (printf)

file :: String -> String -> String
file = printf "input/Day%s.part%s.txt"