{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The second block of the first home work is dedicated to 'Foldable'
-- instance.
module Block2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty (..))


-- | Splits a given 'Foldable' instance on a given separator.
splitOn :: forall a t . (Foldable t, Eq a) => a -> t a -> NonEmpty [a]
splitOn splitter = foldr splitWith ([] :| [])
  where
    splitWith :: Eq a => a -> NonEmpty [a] -> NonEmpty [a]
    splitWith el (x :| xs) =
      if el == splitter
      then [] :| (x:xs)
      else (el:x) :| xs

-- | Concatenates a given list inserting a given separator between elements.
joinWith :: forall a . a -> NonEmpty [a] -> [a]
joinWith splitter = foldr1 insertWith
  where
    insertWith :: [a] -> [a] -> [a]
    insertWith el list = el ++ splitter:list
