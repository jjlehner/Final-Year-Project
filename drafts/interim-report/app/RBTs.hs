module RedBlackSet (RedBlackSet) where

data Colour = Red | Black deriving (Show)
data RedBlackSet a = E | T {
    colour :: Colour,
    left :: RedBlackSet a,
    value :: a,
    right :: RedBlackSet a
} deriving (Show)

balance Black (T Red (T Red a x b) y c) z d = T Red (T Black a x b) y (T Black c z d)
balance Black (T Red a x (T Red b y c)) z d = T Red (T Black a x b) y (T Black c z d)
balance Black a x (T Red (T Red b y c) z d) = T Red (T Black a x b) y (T Black c z d)
balance Black a x (T Red b y (T Red c z d)) = T Red (T Black a x b) y (T Black c z d)
balance color a x b = T color a x b

empty = E
member _ E = False
member x (T _ a y b) =
    if x < y then member x a
    else if x > y then member x b
    else True

insert :: Ord a => a -> RedBlackSet a -> RedBlackSet a
insert x (T color a y b) = T Black q w e
    where
        ins E = T Red E x E
        ins (T color a y b) =
            if x < y then balance color (ins a) y b
            else if x > y then balance color a y (ins b)
            else (T color a y b)
        T _ q w e = ins $ T color a y b
insert x E = T Black E x E
