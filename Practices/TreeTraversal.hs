module TreeTraversal where

import Data.Monoid
import Data.Foldable

-- | Define a Tree, Binary Tree is this coontext
data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show
-- Instance of Functor, to use fmap()
instance Functor Tree where
    fmap f (Leaf l)            = Leaf (f l)
    fmap f (Node n left right) = Node (f n) (fmap f left) (fmap f right)
-- Instance of Foldable, to use foldmap()
instance Foldable Tree where
    foldMap f (Leaf l) = f l
    foldMap f (Node n left right) = f n <> foldMap f left <> foldMap f right

tree = Node 4 (Node 5 (Leaf 1) (Leaf 2)) (Leaf 3) 

-- To be implemented:
-- Depth First Search: pre-order, in-order, post-order
-- Breadth First Search: level-order 
preorder, inorder, postorder, levelorder :: Tree a -> [a]

preorder   = undefined

inorder    = undefined

postorder  = undefined

levelorder = undefined
