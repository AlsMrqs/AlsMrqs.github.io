module Integer where

import Data.Char (isNumber)

parse :: [Char] -> Bool
parse [] = False
parse xs = f xs
    where
    f []     = True
    f (x:xs) = if isNumber x then f xs else False

