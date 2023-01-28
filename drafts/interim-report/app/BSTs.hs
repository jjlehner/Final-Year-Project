module BSTs where

data BST a = E | T {
    left :: BST a,
    value :: a,
    right :: BST a
} deriving (Show)

member x E = False
member x (T l v r) =
    if x < v then member x l
    else if x > v then member x r
    else True

insert :: Ord a => a -> BST a -> BST a
insert x E = T {left=E, value=x, right=E}
insert x (T l v r) =
    if x < v then T{left=insert x l, value=v, right=r}
    else if x > v then T{left=l, value=v, right=insert x r}
    else (T l v r)