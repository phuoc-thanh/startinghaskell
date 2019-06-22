module TreeTraversal where

-- | Define a Tree, Binary Tree is this coontext
data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a) deriving Show
-- Instance of Functor, to use fmap()
instance Functor Tree where
    fmap f (Leaf l)            = Leaf (f l)
    fmap f (Node n left right) = Node (f n) (fmap f left) (fmap f right)
-- Instance of Foldable, to use foldmap()
instance Foldable Tree where
    foldMap f (Leaf l) = f l
    foldMap f (Node n left right) = f n <> foldMap f left <> foldMap f right

-- Sample tree
--         1
--        / \
--       /   \
--      /     \
--     2       3
--    / \     /
--   4   5   6
--  /       / \
-- 7       8   9

tree = Node 1 (Node 2 (Node 4 (Leaf 7) Empty) (Leaf 5))
              (Node 3 (Node 6 (Leaf 8) (Leaf 9)) Empty)

-- To be implemented:
-- Depth First Search: pre-order, in-order, post-order
-- Breadth First Search: level-order

-- preorder:    1 2 4 7 5 3 6 8 9
-- inorder:     7 4 2 5 1 8 6 9 3
-- postorder:   7 4 5 2 8 9 6 3 1
-- levelorder:  1 2 3 4 5 6 7 8 9

preorder, inorder, postorder, levelorder :: Tree a -> [a]

preorder Empty        = []
preorder (Leaf a)     = [a]
preorder (Node a l r) = a : preorder l ++ preorder r

inorder Empty  = []
inorder (Leaf a) = [a]
inorder (Node a l r) = inorder l ++ [a] ++ inorder r

postorder Empty  = []
postorder (Leaf a) = [a]
postorder (Node a l r) = postorder l ++ postorder r ++ [a]

levelorder  = undefined
