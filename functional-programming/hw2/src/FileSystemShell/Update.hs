---------------------------------------------------------------------------
-- |
-- Module      : FileSystemShell.Update
-- Description : Disk dumping
-- Copyright   : (c) Simon Naumov, 2020
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
--
-- This module is used for dumping the current state of file system to the
-- disk.
---------------------------------------------------------------------------

module FileSystemShell.Update
  ( dumpState
  ) where

import Control.Monad (void, when)
import qualified Data.HashMap.Lazy as HashMap (toList, traverseWithKey)
import Data.List (inits, (\\))
import Data.List.Extra (dropEnd1)
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist,
                         listDirectory, removeFile, removePathForcibly,
                         setPermissions)
import System.FilePath.Posix (joinPath, splitDirectories, (</>))

import FileSystemShell.Types (DirectoryVersion (..), FileTree (..),
                              FileVersion (..), Revision (..), ShellState (..))
import FileSystemShell.VersionControlSystem (logFile, revFolder, vcsFolder)


-- | Dumps the shell state to the disk. Because 'FileSystemShell.Scan.scan'
-- refuses to evaluate if a given directory or a file is not readable, that is
-- caused by file system concept, it has to know its files' and directories'
-- contents, so there is an invariant that all files and directories that were
-- in the scanned directory are readable. Therefore, it is possible to create
-- or write into files or directories, that does exist, and delete those, which
-- does not.
dumpState :: ShellState -> IO ()
dumpState ShellState{ currentFileTree = ft, globalPath = gp } = do
  putStrLn $ "exporting in \"" ++ gp ++ "\"..."
  dumpTree gp ft
  findVcs  (joinPath . dropEnd1 . splitDirectories $ gp) ft

dumpTree :: FilePath -> FileTree -> IO ()
dumpTree path Directory{ dContent = dc, dPermissions = perms } = do
  dumpDirectory path
  content <- listDirectory path
  mapM_ (removePath . (path </>)) $ content \\ map fst (HashMap.toList dc)
  void $ HashMap.traverseWithKey (dumpTree . (path </>)) dc
  setPermissions path perms
dumpTree path File{ fContent = fc, fPermissions = fp } = do
  dumpFile path
  writeFile path fc
  setPermissions path fp

removePath :: FilePath -> IO ()
removePath path = do
  isFile <- doesFileExist path
  isDir  <- doesDirectoryExist path
  if isFile
  then removeFile path
  else
    if isDir
    then removePathForcibly path
    else putStrLn $ "cannot delete undefined path " ++ path

findVcs :: FilePath -> FileTree -> IO ()
findVcs _      File{} = return ()
findVcs path d@Directory{ dVersion = Just _, dName = dn } =
  dumpVcs d $ path </> dn
findVcs path   Directory{ dContent = dc, dName = dn } =
  mapM_ (findVcs $ path </> dn) dc

dumpVcs :: FileTree -> FilePath -> IO ()
dumpVcs d@Directory{ dVersion = Just (DirectoryVersion strs) } path = do
  let vcsPath = path </> vcsFolder
  dumpDirectory vcsPath
  dumpFile $ vcsPath </> logFile
  dumpDirectory $ vcsPath </> revFolder
  writeFile (vcsPath </> logFile) $ unlines strs
  mapM_ (dumpRevs $ vcsPath </> revFolder) $ dContent d
dumpVcs _ _ = return ()

dumpRevs :: FilePath -> FileTree -> IO ()
dumpRevs path Directory{ dContent = dc, dName = dn } =
  mapM_ (dumpRevs $ path </> dn) dc
dumpRevs path File{ fName = fn, fVersion = Just (FileVersion revs) } = do
  let newPath = path </> fn
  createDirectories newPath
  let newPair (a, b) = (newPath </> show a ++ ". " ++ rMessage b, rContent b)
  mapM_ (uncurry writeFile . newPair) $ zip [(0 :: Int)..] revs
dumpRevs _ _ = return ()

createDirectories :: FilePath -> IO ()
createDirectories path = mapM_ (createIfNeeded . joinPath) . tail .
  inits $ splitDirectories path
  where
    createIfNeeded :: FilePath -> IO ()
    createIfNeeded fp = do
      exist <- doesDirectoryExist fp
      if exist
      then return ()
      else createDirectory fp

dumpDirectory :: FilePath -> IO ()
dumpDirectory path = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path
  when isFile $ removeFile path
  when (not isDir) $ createDirectory path
  return ()

dumpFile :: FilePath -> IO ()
dumpFile path = do
  isDir <- doesDirectoryExist path
  when isDir $ removePathForcibly path
  return ()
