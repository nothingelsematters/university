{-# LANGUAGE InstanceSigs #-}

-- | The fourth block of the first home work is dedicated to 'Functor' and
-- his friends.
module Block4
  ( NonEmpty(..)
  , Tree(..)
  , stringSum
  ) where

import Data.Function (on)
import Text.Read (readMaybe)


-- | Given a string this function tries to parse integers and sum them up, it
-- returns 'Nothing' on parse failure.
stringSum :: String -> Maybe Int
stringSum = fmap sum . traverse (readMaybe :: String -> Maybe Int) . words

-- | A simple tree data structure.
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving (Eq, Show)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a)            = Leaf $ f a
  fmap f (Branch left right) = on Branch (fmap f) left right

instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  Leaf f     <*> Leaf a     = Leaf $ f a
  Leaf f     <*> Branch l r = on Branch (Leaf f <*>) l r
  Branch l r <*> a          = on Branch (<*> a) l r

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf a)            = f a
  foldMap f (Branch left right) = on mappend (foldMap f) left right

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Branch left right) =
    Branch <$> traverse f left <*> traverse f right

-- | A list, that cannot be empty, data structure.
data NonEmpty a = a :| [a] deriving (Eq, Show)

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (h :| t) = f h :| fmap f t

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure = (:| [])

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (f :| fs) <*> (x :| xs) = f x :| (fmap f xs ++ (fs <*> (x:xs)))

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (x :| xs) = f x `mappend` foldMap f xs

instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (x :| xs) = (:|) <$> (f x) <*> traverse f xs

instance Monad NonEmpty where
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (x :| xs) >>= f = foldl conc (f x) $ map f xs
    where
      (y :| ys) `conc` (z :| zs) = y :| (ys ++ z : zs)
