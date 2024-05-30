module Manage where

import Data.Maybe       (fromJust)
import Stack

data Tree a = Node a (Tree a) (Tree a) | Leaf a
    deriving (Show)

type Operator = [Char]

manage :: [String] -> Stack Operator -> Stack (Tree [Char]) -> Stack (Tree [Char])
manage [] stackOp stackTr = if size stackOp == 0
    then stackTr
    else let
        (justOp, stackOp1) = pop stackOp
        operator           = fromJust justOp 
        stackTr1           = evaluate operator stackTr
        in manage [] stackOp1 stackTr1
manage (x:xs) stackOp stackTr
    | isOperator x = if size stackOp == 0
        then manage xs (push x stackOp) stackTr
        else let 
            (justOp, stackOp1) = pop stackOp
            operator           = fromJust justOp
            in if not $ precedence operator
                then manage xs (push x stackOp) stackTr
                else let
                    stackTr1 = evaluate operator stackTr
                    stackOp2 = push x stackOp1
                    in manage xs stackOp2 stackTr1
    | otherwise = manage xs stackOp (push (Leaf x) stackTr)

isOperator :: [Char] -> Bool
isOperator = (`elem` ["+","-","*","/"])

precedence :: Operator -> Bool
precedence = (`elem` ["*","/"])

evaluate :: Operator -> Stack (Tree [Char]) -> Stack (Tree [Char])
evaluate op stackTr = let
    (justTr1, stackTr1) = pop stackTr
    (justTr2, stackTr2) = pop stackTr1
    tree1               = fromJust justTr1
    tree2               = fromJust justTr2
    in push (Node op tree2 tree1) stackTr2

