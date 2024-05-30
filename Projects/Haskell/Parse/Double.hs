module Double where

import Queue

parse :: [Char] -> Bool
parse [] = False
parse xs = f Queue xs
    where
    f q []     = True
    f q (x:xs) | isNumber x = f (enq x q) xs
               | 

