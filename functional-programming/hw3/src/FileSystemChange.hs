------------------------------------------------------------
-- |
-- Module      : FileSystemChange
-- Description : Lenses for file system changing
-- Copyright   : (c) Simon Naumov, 2020
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
-- Language    : Haskell2010
--
-- This module consists of different functions using lenses
-- used for 'FileSystem' changing.
------------------------------------------------------------
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Rank2Types     #-}

module FileSystemChange
  ( changeExtentions
  , getAllNames
  , getPath
  , move
  , removeIfEmpty
  ) where

import Lens.Micro (SimpleFold, Traversal', each, folding, mapped, non, over, to, (%~), (^.), (^..),
                   (^?))
import System.FilePath.Posix ((-<.>), (</>))

import FileSystemLens (FileSystem (..), content, name, _File)
import FileSystemTraversal (cd)

-- | Change file extensions in a directory (not recursively).
changeExtentions :: String -> FileSystem -> FileSystem
changeExtentions ext = content . mapped . _File . name %~ (-<.> ext)

-- | Get all file and directory names recursively.
getAllNames :: FileSystem -> [FilePath]
getAllNames fs = fs ^. name : fs ^? content ^. non [] . each . to getAllNames

-- | Remove directory, only if it is empty.
removeIfEmpty :: FilePath -> FileSystem -> FileSystem
removeIfEmpty dir = over content $ filter \x ->
  x ^. name /= dir || Just [] /= x ^? content

-- | Get full path to a file with its name resolved by 'FileSystem' root.
-- It is a 'name' alias, actually, used for 'move' combination convenience.
-- It should be used with 'move' as follows:
--
-- >>> myDir ^? move "A" . move "B" . getPath  -- myDir is labeled by "root"
-- Just "root/A/B/"
--
getPath :: Traversal' FileSystem FilePath
getPath = name

-- | Changes the directory saving the breadcrumbs on the way. It should be used
-- with 'getPath'.
move :: FilePath -> SimpleFold FileSystem FileSystem
move dir = folding \d -> d ^.. cd dir . to (name %~ (d ^. name </>))
