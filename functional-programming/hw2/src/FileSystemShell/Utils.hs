{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------
-- |
-- Module      : FileSystemShell.Utis
-- Description : Useful utils functions
-- Copyright   : (c) Simon Naumov, 2020
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
--
-- This module consists of useful function to be used in this file system
-- implementation.
--------------------------------------------------------------------------

module FileSystemShell.Utils
  ( -- * Common 'FileTree' actions
    descend
  , find
  , findDirectory
  , findFile
  , getCurrentDirectory
  , maybeToAction
  , noMsg
  , resolvePath
  , updateDirectory

   -- * Common version control system actions
  , getRevisions
  , updateVcs

   -- * Assertions
  , assertDirectory
  , assertFile
  , assertReadable
  , assertWritable
  , validateRevision
  ) where

import Control.Monad (void)
import Control.Monad.State (get, put)
import Data.Function (on)
import qualified Data.HashMap.Lazy as HashMap (insert, lookup)
import System.Directory (readable, writable)
import System.FilePath.Posix (joinPath, normalise, splitDirectories)

import FileSystemShell.Error (directoryExpected, fileExpected, liftThrow,
                              notFound, notInVcs, notReadable, notWritable)
import FileSystemShell.Types (Action, Command, DirectoryVersion (..),
                              FileTree (..), FileVersion (..), Message,
                              Revision (..), ShellState (..))


-- | Creates a command out of action with no message.
noMsg :: Action a -> Command
noMsg = fmap $ const Nothing

-- | Raises a 'Maybe' instance to an 'Action'
maybeToAction :: Message -> Maybe a -> Action a
maybeToAction msg = maybe (liftThrow msg) return

-- | Returns current directory to perform action on.
getCurrentDirectory :: Action FileTree
getCurrentDirectory = do
  ShellState{ currentFileTree = ft, currentFolder = cwd } <- get
  descend ft cwd

-- | Descends in 'FileTree' by given directories.
descend :: FileTree -> [FilePath] -> Action FileTree
descend a [] = return a
descend (Directory{ dContent = content }) (x:xs) =
  maybe notFound return (HashMap.lookup x content) >>= flip descend xs
descend _ _ = notFound

-- | Resolves 'FilePath' with the current working directory of 'ShellState'.
resolvePath :: FilePath -> Action [FilePath]
resolvePath path = do
  ShellState{ currentFolder = cwd } <- get
  let dirList = removeDot . splitDirectories . normalise $ path
  let roll = length . takeWhile (== "..") $ dirList
  if roll > length cwd
  then liftThrow "cannot go above the scanned directory"
  else return $ take (length cwd - roll) cwd ++ dropWhile (== "..") dirList
  where
    removeDot :: [FilePath] -> [FilePath]
    removeDot (".":xs) = xs
    removeDot  xs      = xs

-- | Tries to find a given path ('FileTree' node).
find :: FilePath -> Action FileTree
find path = do
  ShellState{ currentFileTree = ft } <- get
  resolvePath path >>= descend ft

-- | Tries to find a given file path ('File').
findFile :: FilePath -> Action FileTree
findFile = validateFound $ \case
  f@File{} -> return f
  _        -> fileExpected

-- | Tries to find  given directory path ('Directory').
findDirectory :: FilePath -> Action FileTree
findDirectory = validateFound $ \case
  d@Directory{} -> return d
  _             -> directoryExpected

validateFound :: (FileTree -> Action FileTree) -> FilePath -> Action FileTree
validateFound validator path = find path >>= validator

-- | Updates the parent directory of a given path with a given function.
updateDirectory
  :: (String -> FileTree -> Action FileTree)
  -> FilePath
  -> Command
updateDirectory f path = noMsg $ do
  state@ShellState{ currentFileTree = ft } <- get
  way <- resolvePath path >>= validateNotNull
  let (dir, file) = (init way, last way)
  newFileTree <- update file ft dir
  put state{ currentFileTree = newFileTree }
  where
    update :: String -> FileTree -> [FilePath] -> Action FileTree
    update _ File{} _ = directoryExpected
    update file d@Directory{} [] = f file d
    update file d@Directory{ dContent = dc } (x:xs) = do
      found <- maybeToAction "path not found" $ x `HashMap.lookup` dc
      updated <- update file found xs
      let (amountDelta, sizeDelta) = diffProperties updated found
      return d
        { dContent = HashMap.insert x updated dc
        , dFileAmount = dFileAmount d + amountDelta
        , dSize = dSize d + sizeDelta
        }

    diffProperties :: FileTree -> FileTree -> (Integer, Integer)
    diffProperties dl dr = (on (-) dFileAmount dl dr, on (-) dSize dl dr)

validateNotNull :: [FilePath] -> Action [FilePath]
validateNotNull [] = liftThrow "empty path"
validateNotNull xs = return xs

-- | Updates version control system with a given function on a given path and
-- logs this action with the given message prefix.
updateVcs :: String -> (FileTree -> Action FileTree) -> FilePath -> Command
updateVcs msg updater path = do
  void $ updateVcsSilently updater path
  logVcsChange msg path

updateVcsSilently :: (FileTree -> Action FileTree) -> FilePath -> Command
updateVcsSilently updater path = noMsg $ do
  state <- get
  way <- resolvePath path
  newFileTree <- recursiveJump way $ currentFileTree state
  put state{ currentFileTree = newFileTree }
  where
    recursiveJump :: [FilePath] -> FileTree -> Action FileTree
    recursiveJump (x:xs) d@Directory{ dContent = dc } =
      case x `HashMap.lookup` dc of
        Nothing -> notFound
        Just ft -> do
          newFileTree <- recursiveJump xs ft
          return d{ dContent = HashMap.insert x newFileTree dc }
    recursiveJump [] ft = updater ft
    recursiveJump _  _  = notFound

logVcsChange :: String -> FilePath -> Command
logVcsChange str path = noMsg $ do
  state <- get
  way <- resolvePath path
  newFileTree <- logDive way (joinPath way) $ currentFileTree state
  put state{ currentFileTree = newFileTree }
  where
    logDive :: [FilePath] -> FilePath -> FileTree -> Action FileTree
    logDive _ root d@Directory{ dVersion = Just (DirectoryVersion dv) } = do
      return d
        { dVersion =
            Just (DirectoryVersion $ dv ++ [str ++ " \"" ++ root ++ "\""])
        }
    logDive (x:xs) root d@Directory{ dContent = dc } =
      case x `HashMap.lookup` dc of
        Nothing  -> notFound
        Just dir -> do
          newDir <- logDive xs root dir
          return d{ dContent = HashMap.insert x newDir dc }
    logDive _ _ _ = notInVcs

-- | Assertion, that a given node is a directory.
assertDirectory :: String -> FileTree -> Either String ()
assertDirectory _ Directory{} = return ()
assertDirectory name _ = Left $
  "somehow " ++ name ++ " folder turns out to be a file"

-- | Assertion, that a given node is a file.
assertFile :: String -> FileTree -> Either String ()
assertFile _ File{} = return ()
assertFile name _ = Left $
  "somehow " ++ name ++ " file turns out to be a file"

-- | Validates the revision: checks, whether the revision with the given number
-- exists or not.
validateRevision :: FileTree -> Int -> Action Revision
validateRevision f rev = do
  vs <- getRevisions f
  if length vs > rev
  then return $ vs !! rev
  else liftThrow $ "there is no such revision: " ++ show rev

-- | Gets revisions out of a 'File' node and gives error message if fails.
getRevisions :: FileTree -> Action [Revision]
getRevisions File{ fVersion = Just (FileVersion vs) } = return vs
getRevisions Directory{}                              = fileExpected
getRevisions _                                        = notInVcs

-- | Asserts, that a node is readable.
assertReadable :: FileTree -> Action FileTree
assertReadable      f@File{ fPermissions = fp } | readable fp = return f
assertReadable d@Directory{ dPermissions = dp } | readable dp = return d
assertReadable _                                = notReadable

-- | Asserts, that a node is writable.
assertWritable :: FileTree -> Action FileTree
assertWritable      f@File{ fPermissions = fp } | writable fp = return f
assertWritable d@Directory{ dPermissions = dp } | writable dp = return d
assertWritable _                                = notWritable
