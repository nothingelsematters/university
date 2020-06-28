-----------------------------------------------------------------------
-- |
-- Module      : FileSystemTraversal
-- Description : Traversals for 'FileSystem'
-- Copyright   : (c) Simon Naumov, 2020
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
-- Language    : Haskell2010
--
-- This module consists of common useful traversals for 'FileSystem'.
-----------------------------------------------------------------------
{-# LANGUAGE Rank2Types #-}

module FileSystemTraversal
  ( cd
  , file
  , ls
  ) where

import Lens.Micro (Traversal', each, filtered, (^.))

import FileSystemLens (FileSystem (..), content, name, _Directory, _File)


children :: Traversal' FileSystem FileSystem
children = content . each

-- | A special 'Lens.Micro.Traversal', that emulates changing a directory of a
-- folder's subdirectory. It has type: @'cd' :: 'Traversal'' 'FileSystem'
-- 'FileSystem'@. That is actually: @'cd' :: 'Lens.Micro.Traversal' 'FileSystem'
-- 'FileSystem' 'FileSystem' 'FileSystem'@. Which actually extends as follows:
-- @'cd' :: 'Applicative' f => ('FileSystem' -> f 'FileSystem') -> 'FileSystem'
-- -> f 'FileSystem'@.
cd :: FilePath -> Traversal' FileSystem FileSystem
cd dir = children . _Directory . filtered ((== dir) . (^. name))

-- | A special 'Lens.Micro.Traversal', that shows a file name, if you can
-- find it in the "current" directory.
file :: FilePath -> Traversal' FileSystem FilePath
file target = children . _File . name . filtered (== target)


-- | A special 'Lens.Micro.Traversal', that lists the directory.
ls :: Traversal' FileSystem FilePath
ls = children . name
