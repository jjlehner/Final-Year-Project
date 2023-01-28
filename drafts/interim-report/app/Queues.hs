module Queues where
import Prelude hiding (head, tail)

data Queue a = Queue{
    lenf :: Int,
    f :: [a],
    lenr :: Int,
    r :: [a]
} deriving (Show)

check lenf f lenr r =
    if lenr <= lenf then Queue lenf f lenr r
    else Queue (lenf + lenr) (f ++ reverse r) 0 []

empty = Queue 0 [] 0 []
isEmpty (Queue lenf f lenr r) = (lenf == 0)

snoc (Queue lenf f lenr r) x = check lenf f (lenr+1) (x:r)

head (Queue _ [] _ _) = error "Empty Queue"
head (Queue _ ( x : _ ) _ _ ) = x

tail (Queue _ [] _ _) = error "Empty Queue"
tail (Queue lenf (x:fprime) lenr r) = check (lenf - 1) fprime lenr r
