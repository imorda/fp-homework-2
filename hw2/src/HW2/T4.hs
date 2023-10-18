module HW2.T4
  ( DotString (..),
    Fun (..),
    Inclusive (..),
    ListPlus (..),
  )
where

data ListPlus a = a :+ ListPlus a | Last a
  deriving (Show)

infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (aL :+ aR) b = aL :+ (aR <> b)
  (<>) (Last a) b   = a :+ b

data Inclusive a b = This a | That b | Both a b
  deriving (Show)

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (This a) <> (This b)         = This $ a <> b
  (That a) <> (That b)         = That $ a <> b
  (This a) <> (That b)         = Both a b
  (That a) <> (This b)         = Both b a
  (This a) <> (Both bL bR)     = Both (a <> bL) bR
  (That a) <> (Both bL bR)     = Both bL (a <> bR)
  (Both bL bR) <> (This a)     = Both (bL <> a) bR
  (Both bL bR) <> (That a)     = Both bL (bR <> a)
  (Both aL aR) <> (Both bL bR) = Both (aL <> bL) (aR <> bR)

newtype DotString = DS String
  deriving (Show)

instance Semigroup DotString where
  DS "" <> DS b = DS b
  DS a <> DS "" = DS a
  DS a <> DS b  = DS $ a <> "." <> b

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  F a <> F b = F $ b . a

instance Monoid (Fun a) where
  mempty = F id
