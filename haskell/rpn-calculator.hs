import Data.Bool (bool)
import Data.Char (isNumber)

data Stack a = Stack [a] deriving (Show)

main :: IO ()
main = do
    putStr "RPN Calculator: "
    frame <- getLine >>= return . words
    if parsing frame 0 then 
        print $ start frame (Stack [])
    else putStrLn "Invalid input!"
    main

type Frame = [String]

parsing :: Frame -> Int -> Bool
parsing []     1 = True
parsing []     _ = False
parsing (x:xs) n
    | not $ operator x = bool False (parsing xs $ n+1) $ number x
    | otherwise        = bool False (parsing xs $ n-1) $ n >= 2

operator :: [Char] -> Bool
operator = (`elem` ["+","-","*","/"])

push :: Double -> Stack Double -> Stack Double
push x (Stack s) = Stack (x:s)

pop :: Stack Double -> (Double, Stack Double)
pop (Stack (x:xs)) = (x, Stack xs)

number :: [Char] -> Bool
number = all ((==) True) . map isNumber

start :: Frame -> Stack Double -> (Frame, Stack Double)
start []     stack = ([],stack)
start (x:xs) stack = start xs $ evaluate x stack

evaluate :: [Char] -> Stack Double -> Stack Double
evaluate x stack | not $ operator x = push (read x) stack
evaluate x stack = let
    (arg1,stack1) = pop stack
    (arg2,stack2) = pop stack1
    in push (apply x arg1 arg2) stack2

apply :: [Char] -> Double -> Double -> Double
apply "+" n m = (+) n m
apply "-" n m = (-) n m
apply "*" n m = (*) n m
apply "/" n m = (/) n m

