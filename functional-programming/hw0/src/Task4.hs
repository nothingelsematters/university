{-|
Module      : Task4
Description : Fourth task of zero home work
Copyright   : (c) Simon Naumov, 2020
License     : MIT
Maintainer  : daretoodefy@gmail.com
Stability   : experimental

This module defines functions from fourth homework task. Defined
functions demonstrate how to implement different functions using 'fix'
function.
-}
module Task4
  ( factorial
  , fibonacci
  , iterateElement
  , mapFix
  ) where

import Data.Function (fix, on)

-- | Returns an infinite list consisting of repeated given argument.
-- Prelude's 'repeat' analogue.
iterateElement :: a -> [a]
iterateElement = fix . (:)

-- | Count the n-th fibonacci number.
fibonacci :: Integer -> Integer
fibonacci =
  fix $ \rec n -> if n <= 1
    then 1
    else on (+) rec (n - 1) (n - 2)

-- | Count factorial mathematical function.
factorial :: Integer -> Integer
factorial =
  fix $ \rec n -> if n <= 1
    then 1
    else n * rec (n - 1)

-- | Returns a list obtained by applying a given function to each element of
-- a given list. Prelude's 'map' analogue.
mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix $ const map
