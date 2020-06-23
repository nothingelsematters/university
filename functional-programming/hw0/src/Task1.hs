{-# LANGUAGE TypeOperators #-}

{-|
Module      : Task1
Description : First task of zero home work
Copyright   : (c) Simon Naumov, 2020
License     : MIT
Maintainer  : daretoodefy@gmail.com
Stability   : experimental
-}
module Task1
  ( associator
  , distributivity
  , eitherAssoc
  , type (<->)
  ) where

-- | Illustrate distributivity rule being applied to 'Either'.
distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a)       = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

-- | Illustrate associativity rule.
associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

-- | Type operator giving a pair of functions turning first type to second and
-- vice versa.
type (<->) a b = (a -> b, b -> a)

-- | Demonstrates associativity rule applied to 'Either' and usage
-- of '<->' type operator.
eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (to, from)
  where
    to   = either (Left . Left) $ either (Left . Right) Right
    from = either (either Left (Right . Left)) (Right . Right)
