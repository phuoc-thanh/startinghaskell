-- Monoid Practices
module TypeClasses where
import Data.Monoid hiding (Sum, Product)
-- import Prelude hiding (Sum)

-- mappend, mconcat, mempty
-- m = mappend [1,2,3] [2,3,4]
-- em = mappend mempty [3,4,5]
-- c = mconcat [[1,2,3],[3,4,5,6],[6]]

-- Monoid Reference
-- https://en.wikibooks.org/wiki/Haskell/Monoids
-- https://en.wikibooks.org/wiki/Haskell/Foldable
-- http://learnyouahaskell.com/functors-applicative-functors-and-monoids
-- https://www.schoolofhaskell.com/user/mgsloan/monoids-tour

mthreeConcat :: Monoid m => m -> m -> m -> m
mthreeConcat a b c = a <> b <> c

-- / Monoid under addition
newtype Sum a = Sum { getSum :: a }
    deriving (Eq, Ord, Show)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    mappend (Sum x) (Sum y) = Sum (x + y)

-- / Monoid under multiplication
newtype Product a = Product { getProduct :: a }
    deriving (Eq, Ord, Show)

instance Num a => Monoid (Product a) where
    mempty = Product 1
    mappend (Product x) (Product y) = Product (x*y)

-- / Functor

data Tree a = Leaf a | Branch (Tree a) (Tree a)
    deriving (Show)

instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

-- fmap (2*) (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)) = Branch (Branch (Leaf 2) (Leaf 4)) (Leaf 6)
 