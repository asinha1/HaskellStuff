{- bst.hs : Binary Search Tree Data Structure
 -
 -}

module BST
( BST(..)
, empty
, singleton
, insert
) where

data BST a = EmptyBST | Node a (BST a) (BST a) deriving (Show, Read, Eq)

empty :: (Ord a) => () -> BST a
empty _ = EmptyBST

singleton :: (Ord a) => a -> BST a
singleton x = Node x EmptyBST EmptyBST

insert :: (Ord a) => a -> BST a -> BST a
insert x EmptyBST = singleton x
insert x (Node a l r)
  | x == a = Node x l r
  | x < a  = Node a (insert x l) r
  | x > a  = Node a l (insert x r)

find :: (Ord a) => a -> BST a -> Bool
find x EmptyBST = False
find x (Node a l r)
  | x == a = True
  | x < a  = find x l
  | x > a  = find x r


