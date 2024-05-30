import Data.Bool (bool)
import Data.Char (isNumber)

import Stack as Stack    (Stack(Stack), pop, push)

main :: IO ()
main = do
    putStr "RPN Calculator: "
    frame <- getLine >>= return . words

    if not (parse frame 0) then putStrLn "Sintax error!"
    else print (start frame $ Stack [])

    main

type Frame = [String]

-- todo 
-- error case { parser must return an error specification }
-- write a parser module
parse :: Frame -> Int -> Bool 
parse []     1 = True       
parse []     _ = False       
parse (x:xs) n
    | not (operator x) = bool False (parse xs $ n+1) $ number x 
    | otherwise        = bool False (parse xs $ n-1) $ n >= 2   

operator :: [Char] -> Bool
operator = (`elem` ["+","-","*","/"])

number :: [Char] -> Bool
number = all ((==) True) . map isNumber

start :: Frame -> Stack Double -> (Frame, Stack Double)
start []     stack = ([],stack)
start (x:xs) stack = start xs $ evaluate x stack

evaluate :: [Char] -> Stack Double -> Stack Double
evaluate x stack | not $ operator x = push (read x) stack
evaluate x stack = let
    (Just arg1, stack1) = pop stack
    (Just arg2, stack2) = pop stack1
    in push (apply x arg1 arg2) stack2

apply :: [Char] -> Double -> Double -> Double
apply "+" n m = (+) n m
apply "-" n m = (-) n m
apply "*" n m = (*) n m
apply "/" n m = (/) n m

