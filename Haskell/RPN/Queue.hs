module Queue where

data Queue a = Queue | Item a (Queue a) 
    deriving (Show,Eq)

enq :: a -> Queue a -> Queue a
enq x Queue          = Item x Queue
enq x (Item n queue) = Item n (enq x queue)

deq :: Queue a -> (Queue a, Maybe a)
deq Queue          = (,) Queue Nothing
deq (Item x queue) = (,) queue (Just x) 

