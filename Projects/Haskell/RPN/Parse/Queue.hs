module Queue where

data Queue a = Queue | Item a (Queue a) 
    deriving (Show,Eq)

enq :: a -> Queue a -> Queue a
enq x Queue          = Item x Queue
enq x (Item n queue) = Item n (enq x queue)

deq :: Queue a -> (Queue a, Maybe a)
deq Queue          = (,) Queue Nothing
deq (Item x queue) = (,) queue (Just x) 

empty :: Queue a -> Bool
empty Queue = True
empty _     = False

front :: Queue a -> Maybe a
front Queue          = Nothing
front (Item x Queue) = Just x
front (Item _ queue) = front queue

list :: Queue a -> [a]
list Queue      = []
list (Item x q) = x : list q
