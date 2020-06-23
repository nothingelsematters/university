{-|
Module      : Task6
Description : Sixth task of zero home work
Copyright   : (c) Simon Naumov, 2020
License     : MIT
Maintainer  : daretoodefy@gmail.com
Stability   : experimental

This module defines functions from sixth homework task. It consists of
functions and their weak head normal forms.
-}
module Task6
  ( first
  , first'
  , second
  , second'
  ) where

import Data.Maybe (mapMaybe)
import Task1      (distributivity)

-- | This function was designed in order to study how to deduce weak head
-- normal form, it wasn't supposed for practical use.
first :: (Either String b1, Either String b2)
first  = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- | The weak head normal formal of 'first' expression.
first' :: (Either String b1, Either String b2)
first' = (Left str, Left str)
  where
    str = "harold" ++ " hide " ++ "the " ++ "pain"

-- | This function was designed in order to study how to deduce weak head
-- normal form, it wasn't supposed for practical use.
second :: Bool
second = null $ mapMaybe foo "pole chudes ochen' chudesno"
  where
    foo :: Char -> Maybe Double
    foo char =
      case char == 'o' of
        True -> Just $ exp pi
        False -> Nothing

-- | The weak head normal formal of 'second' expression.
second' :: Bool
second' = False
