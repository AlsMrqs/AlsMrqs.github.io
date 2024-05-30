module Parse.Double where

import Prelude hiding   (lex)
import Data.Char        (isNumber)
import Data.Bool        (bool)
import Queue

key = ['.']

type Error = [Char]

main :: IO ()
main = do
    putStr "Double.parse: "
    input <- getLine
    if input == "exit" then return ()
    else do
        print $ parse input
        main

parse :: [Char] -> Either [Char] Error
parse str = either (\x -> if rule x then (Left $ concat x) else (Right $ "Error: [ "++str++" ] invalid input!")) Right (scan str)
    where
    scan [] = Right ("Error: scan: empty string")
    scan xs = f Queue xs
    f q str = either (\(a,b) -> if (not $ null a) then f (enq a q) b else (Left $ list q)) Right (lex str)

    lex s = g Queue s
    g q []     = (Left $ (,) (list q) [])
    g q (x:xs) | elem x key = if empty q then (Left $ (,) [x] xs) else (Left $ (,) (list q) (x:xs))
               | isNumber x = g (enq x q) xs
               | otherwise  = (Right $ "Error! lex: [ "++[x]++" ]")

    rule str | length str == 1 = all isNumber . head $ str
             | length str == 3 = (\(x:y:z:[]) -> all isNumber x && elem y [key] && all isNumber z) str 
             | otherwise       = False

