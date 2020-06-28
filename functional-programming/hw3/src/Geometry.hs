-----------------------------------------------------------------------
-- |
-- Module      : Geometry
-- Description : Geometry utils
-- Copyright   : (c) Simon Naumov, 2020
-- License     : MIT
-- Stability   : experimental
-- Portability : portable
-- Language    : Haskell2010
--
-- This module consists of 'Point' data structure definition and useful
-- functions to work wtih it.
-----------------------------------------------------------------------
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Geometry
  ( -- * Types
    Point(..)
  , Polygon

   -- * 'Point' functions
  , crossProduct
  , distance
  , minus
  , plus
  , scalarProduct
  , subArea

   -- * 'Polygon' functions
  , doubleArea
  , perimeter
  ) where

import Data.Function (on)


-- | Two-dimensional integer point data type.
data Point
  = Point
  { x :: {-# UNPACK #-} !Int
  , y :: {-# UNPACK #-} !Int
  } deriving (Eq, Ord, Show)

-- | Type alias for a sequential (by points) polygon definition.
type Polygon = [Point]


withDims :: (a -> a -> b) -> (Int -> Int -> a) -> Point -> Point -> b
withDims end f a b = end (on f x a b) (on f y a b)

-- | Summes up 'Point' dimensions independently.
plus :: Point -> Point -> Point
plus = withDims Point (+)

-- | Calculates 'Point' dimension difference.
minus :: Point -> Point -> Point
minus = withDims Point (-)

-- | Calculates scalar product of points.
scalarProduct :: Point -> Point -> Int
scalarProduct = withDims (+) (*)

-- | Calculates cross product of points.
crossProduct :: Point -> Point -> Int
crossProduct a b = x a * y b - y a * x b

-- | Calculates distance between two points.
distance :: Point -> Point -> Double
distance = (sqrt .) . withDims (+) ((realToFrac .) . ((^ (2 :: Int)) .) . (-))

-- | Calculates the area of subgraph of interval defined by two points.
subArea :: Point -> Point -> Int
subArea a b = on (+) x a b * on (-) y a b

sumWith :: forall a b . Num a => (b -> b -> a) -> [b] -> a
sumWith f xs = wrap 0 xs
  where
    wrap :: Num a => a -> [b] -> a
    wrap _    []              = 0
    wrap !res [final]         = (res +) . f final $! head xs
    wrap !res (left:right:ps) = (wrap . (res +) . f left $! right) (right:ps)

-- | Calculates a perimeter of 'Polygon'.
perimeter :: Polygon -> Double
perimeter = sumWith distance

-- | Calculates a double area of 'Polygon'.
doubleArea :: Polygon -> Int
doubleArea = abs . sumWith subArea

