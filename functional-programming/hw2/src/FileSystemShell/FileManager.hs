-----------------------------------------------------
-- |
-- Module      : FileSystemShell.FileManager
-- Description : File manager commands
-- Copyright   : (c) Simon Naumov, 2020
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains useful file manager commands
-----------------------------------------------------

module FileSystemShell.FileManager
  ( -- * Info commands
    cat
  , fd
  , nodeInfo
  , ls
  , tree

   -- * Editing commands
  , mkdir
  , rm
  , mkFile
  , write

   -- * Editing shell state commands
  , cd
  ) where

import Control.Monad (void, when)
import Control.Monad.State (modify)
import qualified Data.HashMap.Lazy as HashMap (delete, foldr, insert, keys,
                                               lookup, member)
import Data.List (sort)
import Data.List.Extra (dropEnd1)
import System.Directory (searchable)
import System.FilePath.Posix (joinPath, pathSeparator)

import FileSystemShell.Error (fileExpected, liftThrow, notFound)
import FileSystemShell.Types (Command, FileTree (..), ShellState (..),
                              emptyDirectory, emptyFile, getNodeName,
                              showProperties)
import FileSystemShell.Utils (assertReadable, assertWritable, find,
                              findDirectory, findFile, getCurrentDirectory,
                              noMsg, resolvePath, updateDirectory)


-- | Change directory command: changes current directory to a given path. Fails
-- if a given path is a file path, a given path doesn't exist or if a given path
-- is out of indexed folder.
cd :: FilePath -> Command
cd path = noMsg $ do
  void $ findDirectory path
  newCwd <- resolvePath path
  modify (\state -> state{ currentFolder = newCwd })


-- | Lists the given directory path. Fails if a given path is a file path, a
-- given path doesn't exist or if a given path is out of indexed folder.
ls :: FilePath -> Command
ls path = pure . dropEnd1 . unlines . sort . HashMap.keys . dContent <$>
  (findDirectory path >>= assertReadable)

-- | Lists the given directory recursively drawing a tree.
tree :: FilePath -> Command
tree path = pure . show <$> findDirectory path


-- | Outputs the file content. Fails if a file does not exist.
cat :: FilePath -> Command
cat path = pure . fContent <$> (findFile path >>= assertReadable)

-- | Outputs file ot directory info.
nodeInfo :: FilePath -> Command
nodeInfo path = pure . showProperties <$> find path

-- | Searches for file or directory of a given name in current directory or its
-- subdirectories.
fd :: FilePath -> Command
fd path = do
  when (pathSeparator `elem` path) $
    liftThrow "provide only file name, not path with directories"
  pure . dropEnd1 . unlines . fdInternal path <$> getCurrentDirectory
  where
    fdInternal :: FilePath -> FileTree -> [FilePath]
    fdInternal p ft = map joinPath . (++ recursiveStep p ft) $
      if getNodeName ft == p
      then [[p]]
      else []

    recursiveStep :: FilePath -> FileTree -> [[FilePath]]
    recursiveStep _ File{} = []
    recursiveStep _ Directory{ dPermissions = dp } | not (searchable dp) = []
    recursiveStep p d@Directory{ dContent = dc } = map (getNodeName d :)
      . filter (not . null)
      $ HashMap.foldr ((:) . fdInternal p) [] dc

-- | Creates a new empty file in a given path. Fails if path is out of
-- indexed folder.
mkFile :: FilePath -> Command
mkFile = updateDirectory $ \str d -> do
  void $ assertWritable d
  let dc = dContent d
  if str `HashMap.member` dc
  then liftThrow "file already exists"
  else
    return d
      { dContent    = HashMap.insert str (emptyFile str) dc
      , dFileAmount = dFileAmount d + 1
      }

-- | Creates a new empty directory in a given path. Fails if path is out of
-- indexed folder.
mkdir :: FilePath -> Command
mkdir = updateDirectory $ \str d -> do
  void $ assertWritable d
  let dc = dContent d
  if str `HashMap.member` dc
  then liftThrow "directory already exists"
  else return d{ dContent = HashMap.insert str (emptyDirectory str) dc }

-- | Removes a given path: file or directory.
rm :: FilePath -> Command
rm = updateDirectory $ \str d -> do
  let dc = dContent d
  let deleted = HashMap.delete str dc
  case str `HashMap.lookup` dc of
    Nothing -> notFound
    Just File{ fSize = fs } -> return d
      { dContent    = deleted
      , dSize       = dSize d - fs
      , dFileAmount = dFileAmount d - 1
      }
    Just Directory{ dSize = ds, dFileAmount = dfa } -> return d
      { dContent    = deleted
      , dSize       = dSize d - ds
      , dFileAmount = dFileAmount d - dfa
      }

-- | Writes a given string to a given file.
write :: FilePath -> String -> Command
write path str = flip updateDirectory path $ \file d -> do
  void $ assertWritable d
  let dc = dContent d
  case file `HashMap.lookup` dc of
    Nothing -> notFound
    Just (Directory{}) -> fileExpected
    Just  f -> do
      void $ assertWritable f
      let newSize = toInteger $ length str
      let updatedFile = f{ fContent = str, fSize = newSize }
      return d
        { dContent = HashMap.insert file updatedFile dc
        , dSize    = dSize d - (fSize f - newSize)
        }
