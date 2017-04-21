-- Monoid Practices
-- mappend, mconcat, mempty
m = mappend [1,2,3] [2,3,4]
em = mappend mempty [3,4,5]
c = mconcat [[1,2,3],[3,4,5,6],[6]]