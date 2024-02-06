module Parse where

import Fifo 

key = ['+','-','*','/']
num = ['0','1','2','3','4','5','6','7','8','9','.']

type Error = [Char]
type Args = [Char]

dead :: Fifo a -> Bool
dead Fifo = True
dead _    = False

download :: Fifo a -> [a]
download Fifo          = []
download (Item x fifo) = x : download fifo 

lexical :: [Char] -> Fifo Char -> Either [String] Error
lexical []     fifo = Left [download fifo, []]
lexical (x:xs) fifo
    | (==) x ' ' = if dead fifo then lexical xs fifo else Left [download fifo, x:xs]
    | elem x key = if dead fifo then Left [[x], xs]  else Left [download fifo, x:xs]
    | elem x num = lexical xs (enq x fifo)
    | otherwise  = Right ("Error: lex: can't read [ "++[x]++" ]")

