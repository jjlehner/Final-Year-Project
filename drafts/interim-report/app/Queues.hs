module Queues where
import Prelude hiding (head, tail)

class Queue q where
    empty :: q a
    isEmpty :: q a -> Bool
    snoc :: q a -> a -> q a
    tail :: q a -> q a
    head :: q a -> a

data BankersQueue a = BankersQueue{
    lenf :: Int,
    f :: [a],
    lenr :: Int,
    r :: [a]
} deriving (Show)

checkbq lenf f lenr r =
    if lenr <= lenf then BankersQueue lenf f lenr r
    else BankersQueue (lenf + lenr) (f ++ reverse r) 0 []

instance Queue BankersQueue where
    empty = BankersQueue 0 [] 0 []
    isEmpty (BankersQueue lenf f lenr r) = (lenf == 0)
    snoc (BankersQueue lenf f lenr r) x = checkbq lenf f (lenr+1) (x:r)

    head (BankersQueue _ [] _ _) = error "Empty Queue"
    head (BankersQueue _ ( x : _ ) _ _ ) = x

    tail (BankersQueue _ [] _ _) = error "Empty Queue"
    tail (BankersQueue lenf (x:fprime) lenr r) = checkbq (lenf - 1) fprime lenr r

data BadQueue a = BadQueue{
    f :: [a],
    r :: [a]
} deriving (Show)

checkf (BadQueue [] r) = BadQueue (reverse r) []
checkf q = q

instance Queue BadQueue where
    empty = BadQueue [] []
    isEmpty (BadQueue f r) = length f == 0
    snoc (BadQueue f r) x = checkf (BadQueue f (x:r))
    head (BadQueue [] _) = error "Empty Queue"
    head (BadQueue (x:f) r) = x
    tail (BadQueue [] _) = error "Empty Queue"
    tail (BadQueue (x : f) r) = checkf (BadQueue f r)

profileOperation queue =


profileQueue queue =
    let stored = []


