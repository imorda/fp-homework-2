module HW2.T3
  ( epart,
    mcat,
  )
where

import Data.Foldable (fold)

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap fold

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap toTuple
  where
    -- This could work just with a single "fold" method if it somehow could unwrap
    -- the Left value, but unfortunately it only works with the Right one :(
    toTuple (Left left)   = (left, mempty)
    toTuple (Right right) = (mempty, right)
