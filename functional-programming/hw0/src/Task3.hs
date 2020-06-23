{-|
Module      : Task3
Description : Third task of zero home work
Copyright   : (c) Simon Naumov, 2020
License     : MIT
Maintainer  : daretoodefy@gmail.com
Stability   : experimental

This module defines functions from third homework task. Defined functions
demonstrate how to implement common functions using SK basis.
-}
module Task3
  ( composition
  , contraction
  , identity
  , permutation
  , s
  ) where

-- | Apply S combinator.
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- | Function composition. Prelude's '.' analogue.
composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (const s) const

-- | Identity function. Prelude's 'id' analogue.
identity :: a -> a
identity = s const const

-- | Apply a function to two same arguments.
contraction :: (a -> a -> b) -> a -> b
contraction = permutation s identity

-- | Apply a function to (first) two arguments swapped. Prelude's 'flip'
-- analogue.
permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (const composition) s) (const const)
