{-|
Module      : Task2
Description : Second task of zero home work
Copyright   : (c) Simon Naumov, 2020
License     : MIT
Maintainer  : daretoodefy@gmail.com
Stability   : experimental
-}
module Task2
  ( doubleNeg
  , doubleNegElim
  , excludedNeg
  , pierce
  , thirdNegElim
  , Neg
  ) where

import Data.Void     (Void)
import Data.Function ((&))

-- | Negation type: a function turning anything to 'Void'.
type Neg a = a -> Void

-- | Make double negation out of the given argument.
doubleNeg :: a -> Neg (Neg a)
doubleNeg = (&)

-- | Double negation of excluded negation implemented with 'Either'.
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg f = f . Right $ f . Left

-- | Demonstrates pierce's arrow rule and the fact that it can't be inhabited.
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- | It could eliminate double negation out of the given argument, but this
-- type can't be inhabited.
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

-- | Eliminates two of three negations out of the given argument.
thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = flip (.) doubleNeg
