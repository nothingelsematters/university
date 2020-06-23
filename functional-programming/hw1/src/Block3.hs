{-# LANGUAGE InstanceSigs #-}

-- | The third block of the first home work is dedicated to Monoids.
module Block3
  ( Endo(..)
  , Name(..)
  , NonEmpty(..)
  , ThisOrThat(..)
  , eitherConcat
  , maybeConcat
  ) where

import Data.Either (partitionEithers)
import Data.Maybe (catMaybes)


-- | Concatenates only the 'Just' values of a given list.
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = concat . catMaybes

-- | Concatenates 'Left' and 'Right' values separately, returning a pair of
-- monoids.
eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = pairMconcat . partitionEithers
  where
    pairMconcat :: (Monoid a, Monoid b) => ([a], [b]) -> (a, b)
    pairMconcat (a, b) = (mconcat a, mconcat b)

-- | A list, that cannot be empty, data structure.
data NonEmpty a = a :| [a] deriving (Show, Eq)

instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (x :| xs) <> (y :| ys) = x :| (xs ++ y : ys)

-- | A data structure, storing this, that or both values.
data ThisOrThat a b
  = This a
  | That b
  | Both a b
  deriving (Eq, Show)

instance Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  This a   <> This _   = This a
  This a   <> That b   = Both a b
  This a   <> Both _ b = Both a b
  That a   <> That _   = That a
  That a   <> This b   = Both b a
  That b   <> Both a _ = Both a b
  Both a b <> _        = Both a b

-- | Name newtype storing a 'String'.
newtype Name = Name String deriving (Show, Eq)

instance Semigroup Name where
  (<>) :: Name -> Name -> Name
  Name x  <> Name "" = Name x
  Name "" <> Name y  = Name y
  Name x  <> Name y  = Name $ x ++ '.' : y

instance Monoid Name where
  mempty :: Name
  mempty = Name ""

-- | A simple data structure that holds a function of one argument returning the
-- same type.
newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  (<>) :: Endo  a -> Endo a -> Endo a
  Endo x <> Endo y = Endo $ x . y

instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = Endo id
