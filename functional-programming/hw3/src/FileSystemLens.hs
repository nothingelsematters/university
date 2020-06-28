-----------------------------------------------------------------------
-- |
-- Module      : FileSystemLenses
-- Description : Lenses for file system
-- Copyright   : (c) Simon Naumov, 2020
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
-- Language    : Haskell2010
--
-- This module consists of 'FileSystem' data type definition and
-- different Lenses for it.
-----------------------------------------------------------------------
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module FileSystemLens
  ( -- * Type definition
    FileSystem(..)

   -- * 'FileSystem' utils
  , scanDirectory

   -- * Useful lenses
  , content
  , name

   -- * Useful prisms
  , _Directory
  , _File
  )
  where

import Data.Text (pack, replace, unpack)
import Lens.Micro (Lens', Traversal', (^.), (^?!))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath.Posix (takeFileName, (</>))


-- | File system data type. File system can be represented as a directory
-- (a node) and as a file (a leaf). It is better to use automatically generated
-- lenses to access fields.
data FileSystem
  = Directory
  { _name    :: FilePath     -- ^ Directory name, not a full path
  , _content :: [FileSystem] -- ^ Recursive directory content
  }
  | File
  { _name    :: FilePath     -- ^ File name, not a full path
  } deriving Eq

-- | '_name' 'FileSystem' field lens. It can be generated with @microlens-th@
-- package, but to make less dependencies (in studying purposes) it can be
-- written explicitly.
name :: Lens' FileSystem FilePath
name f fs = (\name' -> fs { _name = name' }) <$> f (_name fs)

-- | '_content' 'FileSystem' field lens. It can be generated with @microlens-th@
-- package, but to make less dependencies (in studying purposes) it can be
-- written explicitly.
content :: Traversal' FileSystem [FileSystem]
content f d@Directory{} = (\c' -> d { _content = c' }) <$> f (_content d)
content _ fs            = pure fs

instance Show FileSystem where
  show = showIndented ""
    where
      showIndented :: String -> FileSystem -> String
      showIndented ind f@File{}      = ind <> f ^. name <> "\n"
      showIndented ind d@Directory{} = do
        let replaces  = replace "└── " "    " . replace "├── " "│   "
        let newIndent = unpack . replaces . pack $ ind
        ind <> d ^. name <> "\n" <> showListIndented newIndent (d ^?! content)

      showListIndented :: String -> [FileSystem] -> String
      showListIndented _   []   = ""
      showListIndented ind list =
        concatMap (showIndented (ind <> "├── ")) (tail list)
        <> showIndented (ind <> "└── ") (head list)

-- | Strictly scans the given diretory path, creating an instance of
-- 'FileSystem'.
scanDirectory :: FilePath -> IO FileSystem
scanDirectory dir = do
  !contents <- listDirectory dir >>= scanFileSystems . map (dir </>)
  return $! Directory
    { _name    = takeFileName dir
    , _content = contents
    }

scanFile :: FilePath -> IO FileSystem
scanFile file = return $! File{ _name = takeFileName file }

scanFileSystems :: [FilePath] -> IO [FileSystem]
scanFileSystems = fmap concat . mapM \path -> do
  !isFile      <- doesFileExist      path
  !isDirectory <- doesDirectoryExist path
  if isFile
  then fmap return . scanFile $! path
  else
    if isDirectory
    then fmap return . scanDirectory $! path
    else do
      putStrLn $ "WARNING: skipping \"" <> path <> "\": unknown type"
      return $! mempty

-- | File prism. It is a special traversal, that filters the "input" and remain
-- only 'File' instances.
_File :: Traversal' FileSystem FileSystem
_File f file@File{} = f file
_File _ fs          = pure fs

-- | Directory prism. The same as '_File': it is a special traversal, that
-- filters the "input" and remain only 'File' instances.
_Directory :: Traversal' FileSystem FileSystem
_Directory f dir@Directory{} = f dir
_Directory _ fs              = pure fs
