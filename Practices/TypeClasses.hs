-- Monoid Practices
module TypeClasses where
import Data.Monoid hiding (Sum)
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