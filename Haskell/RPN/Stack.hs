module Stack (Stack(Stack,Item),pop,push) where

data Stack a = Stack | Item a (Stack a) 
    deriving (Show,Eq)

push :: a -> Stack a -> Stack a
push x stack = Item x stack

pop :: Stack a -> (Stack a, Maybe a)
pop Stack          = (,) Stack Nothing
pop (Item a stack) = (,) stack (Just a)

