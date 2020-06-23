{-|
Module      : Task5
Description : Fifth task of zero home work
Copyright   : (c) Simon Naumov, 2020
License     : MIT
Maintainer  : daretoodefy@gmail.com
Stability   : experimental

This module defines functions from fifth homework task, common Church
numerals operations.
-}
module Task5
    ( churchMult
    , churchPlus
    , churchToInt
    , succChurch
    , zero
    , Nat
    ) where

import Control.Monad (ap, liftM2)

-- | Church numeral type.
type Nat a = (a -> a) -> a -> a

-- | Build zero Church numeral.
zero :: Nat a
zero = const id

-- | Increase Church numeral by one.
succChurch :: Nat a -> Nat a
succChurch = ap (.)

-- | Sum up two Church numerals.
churchPlus :: Nat a -> Nat a -> Nat a
churchPlus = liftM2 (.)

-- | Multiply two Church numerals.
churchMult :: Nat a -> Nat a -> Nat a
churchMult = (.)

-- | Convert Church numeral to 'Integer'.
churchToInt :: Nat Integer -> Integer
churchToInt n = n (+ 1) 0
