{-|
Module      : Main
Description : Second practice
Copyright   : (c) Simon Naumov, 2020
License     : MIT
Maintainer  : daretoodefy@gmail.com
Stability   : experimental

This module consists of different tasks given.
-}
module Main
  ( main
  , FName(...)
  ) where

import Data.CaseInsensitive (mk)
import Data.Function        (on)
import Data.Char            (isDigit)
import Text.Read            (readMaybe)

-- Task 2

-- | Yet another kind of a string with uncommon comparing: digit prefixes are
-- compared in the first place and then the rest of a string is compared
-- case insensitively.
newtype FName = FName String

hasDigitPrefix :: String -> Bool
hasDigitPrefix str = not (null str) && isDigit (head str)

readInt :: String -> Maybe Int
readInt = readMaybe

instance Eq FName where
  (==) (FName a) (FName b) = equals (readInt . takeWhile isDigit) a b &&
    equals (mk . dropWhile isDigit) a b
      where
        equals :: Eq a => (b -> a) -> b -> b -> Bool
        equals = on (==)


instance Ord FName where
  (<) (FName a) (FName b) =
    if on (&&) hasDigitPrefix a b
    then case on compare (readInt . takeWhile isDigit) a b of
      LT -> True
      GT -> False
      EQ -> on isLess (dropWhile isDigit) a b
    else a `isLess` b
      where
        isLess :: String -> String -> Bool
        isLess = on (<) mk

  (<=) a b = a < b || a == b

-- Task 3

newtype Box a = Box a
data BoxD a = BoxD a

-- `sumAndLog [-2] loop` returns `Nothing`
sumAndLog :: (Floating a, Ord a) => [a] -> Box a -> Maybe a
sumAndLog as (Box base) = let s = sum as in
  if s < 0
  then Nothing
  else Just (log s / log base)

-- `sumAndLogD [-2] loop` loops
sumAndLogD :: (Floating a, Ord a) => [a] -> BoxD a -> Maybe a
sumAndLogD as (BoxD base) = let s = sum as in
  if s < 0
  then Nothing
  else Just (log s / log base)

-- `sumAndLogD' [-2] loop` returns `Nothing`
sumAndLogD' :: (Floating a, Ord a) => [a] -> BoxD a -> Maybe a
sumAndLogD' as = let s = sum as in
  if s < 0
  then const Nothing
  else \base -> Just (log s / log (unbox base))
    where
      unbox :: BoxD a -> a
      unbox (BoxD a) = a

loop :: a
loop = loop

main :: IO ()
main = pure ()
