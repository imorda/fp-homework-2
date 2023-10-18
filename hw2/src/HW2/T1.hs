module HW2.T1
  ( Tree (..),
    tfoldr,
  )
where

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ acc Leaf = acc
tfoldr f acc (Branch _ left cur right) = tfoldr f (f cur rightRedex) left
  where
    rightRedex = tfoldr f acc right
