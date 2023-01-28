module Structures where

import Prelude hiding (head, tail, (++))

data List a =
    Nil
    | Cons (a, List a) deriving (Show)

head :: List a -> a
head Nil = undefined
head (Cons (x,s)) = x

tail :: List a -> List a
tail Nil = undefined
tail (Cons (x,s)) = s

isEmpty Nil = True
isEmpty _ = False

(++) :: List a -> List a -> List a
(++) xs ys = if isEmpty xs then ys else Cons (head xs, tail xs ++ ys)