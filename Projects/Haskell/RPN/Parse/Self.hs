module Parse.Self where

import Data.Maybe       (fromJust)
import Data.Char        (isNumber)
import Queue
import Stack

data Tree a = Node a (Tree a) (Tree a) | Leaf a | Empty
    deriving (Show)

key = ['+','-','*','/']
num = ['0','1','2','3','4','5','6','7','8','9','.']

type Operator   = [Char]
type Error      = [Char]

main :: IO ()
main = return ()

lex :: [Char] -> Either ([Char],[Char]) Error
lex []  = Left ([],[])
lex str = f Queue str
    where
    f q (x:xs) | (==) x ' ' = if empty q then f q xs         else Left (list q, xs)
               | elem x key = if empty q then Left ([x],xs)  else Left (list q, (x:xs))
               | elem x num = if empty q then f (enq x q) xs else Left (list q, (x:xs))
               | otherwise  = Right ("Error: lex: [ "++[x]++" ] invalid!")

manage :: [String] -> Stack Operator -> Stack (Tree [Char]) -> Stack (Tree [Char])
manage []     stackOp stackTr = if size stackOp == 0 
    then stackTr
    else let
        (stackOp1, stackTr1) = evaluate stackOp stackTr
        in manage [] stackOp1 stackTr1

manage (x:xs) stackOp stackTr
    | isOperator x = if precedence stackOp 
        then let
            (stackOp1, stackTr1) = evaluate stackOp stackTr
            in  manage (x:xs) stackOp1 stackTr1
        else let
            stackOp1 = push x stackOp 
            in manage xs stackOp1 stackTr
    | otherwise = let
        stackTr1 = push (Leaf x) stackTr
        in manage xs stackOp stackTr1

isOperator :: [Char] -> Bool
isOperator x = elem x ["+","-","*","/"]

precedence :: Stack Operator -> Bool
precedence stackOp = if size stackOp == 0 then False
    else let
        (justOp, stackOp1) = pop stackOp
        operator           = fromJust justOp
        in elem operator ["*","/"]

evaluate :: Stack Operator -> Stack (Tree [Char]) -> (Stack Operator, Stack (Tree [Char]))
evaluate stackOp stackTr = let
    (justOp1, stackOp1) = pop stackOp
    operator            = fromJust justOp1
    (justTr1, stackTr1) = pop stackTr
    (justTr2, stackTr2) = pop stackTr1
    tree1               = fromJust justTr1
    tree2               = fromJust justTr2
    stackTr3            = push (Node operator tree2 tree1) stackTr2
    in (stackOp1, stackTr3)

