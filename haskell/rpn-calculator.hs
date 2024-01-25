import Data.Bool (bool)
import Data.Char (isNumber)

data Stack a = Stack [a] deriving (Show)

main :: IO ()
main = do
    putStr "RPN Calculator: "
    frame <- getLine >>= return . words

    if not $ parsing frame 0 then putStrLn "Invalid input!"
    else print $ start frame (Stack [])

    main

type Frame = [String]

parsing :: Frame -> Int -> Bool 
parsing []     1 = True       -- stack must end with (1) element
parsing []     _ = False      -- any other quantity 
parsing (x:xs) n
    | not $ operator x = bool False (parsing xs $ n+1) $ number x -- if [x1..] is number   stack.length (+1)
    | otherwise        = bool False (parsing xs $ n-1) $ n >= 2   -- if [x1..] is operator stack.length (-1)

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

