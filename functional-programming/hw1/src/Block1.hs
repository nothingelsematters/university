{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The first block of the first home work is dedicated to algebraic data
-- types.
module Block1
  ( Nat(..)
  , Tree(..)
  , Weekday(..)
  , (^+)
  , (^-)
  , (^*)
  , (^/)
  , afterDays
  , contains
  , daysToParty
  , nextDay
  , fromList
  , insert
  , isEmpty
  , isEven
  , isWeekend
  , natMod
  , remove
  , size
  ) where

import Data.Foldable (toList)
import Data.Function (on)
import Data.List (elemIndex)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust)

-- | A data type storing information about particular day of a week.
data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

order :: [Weekday]
order = [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

cyclicAt :: [a] -> Int -> a
cyclicAt xs index = xs !! (index `mod` length xs)

instance Eq Weekday where
  (==) :: Weekday -> Weekday -> Bool
  (==) = on (==) show

instance Enum Weekday where
  toEnum :: Int -> Weekday
  toEnum = cyclicAt order

  fromEnum :: Weekday -> Int
  fromEnum element = fromJust $ elemIndex element order

-- | Returns the next day of a week.
nextDay :: Weekday -> Weekday
nextDay = succ

-- | Returns what will be a day of a week after given number of days passed
-- starting at a given day.
afterDays :: Weekday -> Int -> Weekday
afterDays wd interval = cyclicAt order (interval + fromEnum wd)

-- | Returns whether the given day of a week is a weekend.
isWeekend :: Weekday -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | Counts the number of days left til the party.
daysToParty :: Weekday -> Int
daysToParty = length . takeWhile (/= Friday)  . enumFrom

-- | Natural number data structure. It is not an instance of 'Enum' because it
-- doesn't form a ring.
data Nat
  = Z
  | S Nat

instance Enum Nat where
  toEnum :: Int -> Nat
  toEnum x = liftUp $ abs x
    where
      liftUp :: Int -> Nat
      liftUp 0 = Z
      liftUp n = S . liftUp $ n - 1

  fromEnum :: Nat -> Int
  fromEnum Z     = 0
  fromEnum (S s) = 1 + fromEnum s

instance Show Nat where
  show :: Nat -> String
  show = show . fromEnum

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) = on (==) fromEnum

instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  (<=) = binaryOperation (<=)

binaryOperation :: (Int -> Int -> a) -> Nat -> Nat -> a
binaryOperation op = on op fromEnum

natBinaryOperation :: (Int -> Int -> Int) -> Nat -> Nat -> Nat
natBinaryOperation op a = toEnum . binaryOperation op a

-- | 'Nat' sum operator.
infixl 6 ^+
(^+) :: Nat -> Nat -> Nat
(^+) = natBinaryOperation (+)

-- | 'Nat' minus operator.
infixl 6 ^-
(^-) :: Nat -> Nat -> Nat
(^-) = natBinaryOperation (-)

-- | 'Nat' multiplication operator.
infixl 7 ^*
(^*) :: Nat -> Nat -> Nat
(^*) = natBinaryOperation (*)

-- | 'Nat' division operator.
infixl 7 ^/
(^/) :: Nat -> Nat -> Nat
(^/) = natBinaryOperation (div)

-- | 'Nat' modulo division operator.
infixl 7 `natMod`
natMod :: Nat -> Nat -> Nat
natMod = natBinaryOperation (mod)

-- | Checks if the given 'Nat' number is even.
isEven :: Nat -> Bool
isEven Z     = True
isEven (S s) = isEven' False s
  where
    isEven' :: Bool -> Nat -> Bool
    isEven' flag Z     = flag
    isEven' flag (S n) = isEven' (not flag) n

-- | Binary search tree data structure. It stores elements only in nodes and
-- has empty leaves.
data Tree a
  = Leaf
  | Node (NonEmpty a) (Tree a) (Tree a)
  deriving Show

-- Second block, actually. But avoiding orphan instances.
instance Foldable Tree where
  foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
  foldMap _  Leaf = mempty
  foldMap f (Node el left right) =
    foldMap f left `mappend` foldMap f el `mappend` foldMap f right

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ start Leaf = start
  foldr f start (Node el left right) =
    foldr f (foldr f (foldr f start right) el) left

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) = on (==) toList

-- | Checks whether a given tree is empty.
isEmpty :: Ord a => Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | Returns the total amount of elements stored in a tree.
size :: Ord a => Tree a -> Int
size  Leaf                = 0
size (Node xs left right) = length xs + on (+) size left right

descent :: Ord a => (Tree a -> a -> Tree a) -> Tree a -> a -> Tree a
descent f Leaf el = f Leaf el
descent f node@(Node keys@(x :| _) left right) el =
  case compare el x of
    LT -> Node keys (descent f left el) right
    GT -> Node keys left (descent f right  el)
    EQ -> f node el

-- | Checks whether a given tree contains a given element.
contains :: Ord a => Tree a -> a -> Bool
contains Leaf _  = False
contains (Node (x :| _) left right) el =
  case compare el x of
    LT -> contains left  el
    GT -> contains right el
    EQ -> True

-- | Inserts a given element in a given tree.
insert :: Ord a => Tree a -> a -> Tree a
insert = descent insert'
  where
    insert' :: Tree a -> a -> Tree a
    insert'  Leaf                       el = Node (el :| [])   Leaf Leaf
    insert' (Node (x :| xs) left right) el = Node (el :| x:xs) left right

-- | Generates a tree out of a given list.
fromList :: Ord a => [a] -> Tree a
fromList = foldl insert Leaf

-- | Removes a given element from a given tree.
remove :: Ord a => Tree a -> a -> Tree a
remove = descent remove'
  where
    remove' :: forall a . Ord a => Tree a -> a -> Tree a
    remove'  Leaf                           _ = Leaf
    remove' (Node (_ :| x : xs) left right) _ = Node (x :| xs) left right
    remove' (Node  _            left Leaf ) _ = left
    remove' (Node  _            left right) _ = Node newKeys left newRight
      where
        (newKeys, newRight) = leftmost right

        leftmost :: Tree a -> (NonEmpty a, Tree a)
        leftmost  Leaf = undefined
        leftmost (Node k Leaf r) = (k, r)
        leftmost (Node k (Node keys Leaf subtree) r) = (keys, Node k subtree r)
        leftmost (Node keys l r) = (leftmostKeys, Node keys newLeft r)
          where
            (leftmostKeys, newLeft) = leftmost l
