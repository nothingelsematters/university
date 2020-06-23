-----------------------------------------------------
-- |
-- Module      : FileSystemShell.Scan
-- Description : Loading from disk
-- Copyright   : (c) Simon Naumov, 2020
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
--
-- This module is used to load 'FileTree' from disk.
-----------------------------------------------------

module FileSystemShell.Scan
  ( scan
  ) where

import qualified Data.HashMap.Lazy as HashMap (delete, foldrWithKey, fromList,
                                               insert, lookup, map, member,
                                               toList)
import Data.List (intercalate, sort)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import System.Directory (doesDirectoryExist, doesFileExist, getFileSize,
                         getModificationTime, getPermissions, listDirectory)
import System.FilePath (splitDirectories, takeFileName, (</>))
import System.FilePath.Posix (takeExtension)
import qualified System.IO.Strict as IOStrict (readFile)
import Text.Read (readMaybe)

import FileSystemShell.Error (wrongVcsStructure)
import FileSystemShell.Types (DirectoryVersion (..), FileTree (..),
                              FileVersion (..), Message, Revision (..),
                              getNodeName, isDirectory, treeFileAmount,
                              treeSize)
import FileSystemShell.Utils (assertDirectory, assertFile)
import FileSystemShell.VersionControlSystem (logFile, revFolder, vcsFolder)


-- | Tries to load a given directory. Outputs is an error message or a
-- 'FileTree'
scan :: FilePath -> IO (Either Message FileTree)
scan = fmap scanVCS . scanDirectory

scanDirectory :: FilePath -> IO FileTree
scanDirectory path = do
  lstContents <- scanPaths path =<< listDirectory path
  let contents = HashMap.fromList . map (\x -> (getNodeName x, x)) $ lstContents
  let sumMap f = sum . map f $ lstContents
  permissions <- getPermissions path
  return Directory
    { dContent     = contents
    , dVersion     = Nothing
    , dName        = last . splitDirectories $ path
    , dSize        = sumMap treeSize
    , dFileAmount  = sumMap treeFileAmount
    , dPermissions = permissions
    }


scanFile :: FilePath -> IO FileTree
scanFile path = do
  permissions <- getPermissions path
  modificationTime <- getModificationTime path
  size <- getFileSize path
  content <- IOStrict.readFile path
  return File
    { fContent          = content
    , fVersion          = Nothing
    , fName             = takeFileName path
    , fPermissions      = permissions
    , fType             = takeExtension path
    , fModificationTime = modificationTime
    , fSize             = size
    }


scanPaths :: FilePath -> [FilePath] -> IO [FileTree]
scanPaths dir = (fmap catMaybes .) . mapM $ \node -> do
  let path = dir </> node
  isDir  <- doesDirectoryExist path
  isFile <- doesFileExist path
  if isDir
  then return <$> scanDirectory path
  else
    if isFile
    then return <$> scanFile path
    else do
      putStrLn $ "WARNING: skipping \"" ++ path ++ "\", will be deleted on dump"
      return Nothing


scanVCS :: FileTree -> Either Message FileTree
scanVCS f@File{} = return f
scanVCS d@Directory{ dContent = dc } = case vcsFolder `HashMap.lookup` dc of
  Nothing  -> do
    newContent <- sequence $ HashMap.map scanVCS dc
    return d{ dContent = newContent }
  Just vcs -> do
    assertDirectory vcsFolder vcs
    let newContent = HashMap.delete vcsFolder dc
    mapM_ noVcsSubdirectory $ newContent
    patchVCS vcs d{ dContent = newContent }
  where
    noVcsSubdirectory :: FileTree -> Either Message ()
    noVcsSubdirectory File{} = return ()
    noVcsSubdirectory Directory{ dContent = cont } =
      if vcsFolder `HashMap.member` cont
      then Left "vcs subdirectory found, submodules are not supported"
      else return ()


patchVCS :: FileTree -> FileTree -> Either Message FileTree
patchVCS vcs d = patchLogs >>= patchRevs
  where
    patchLogs :: Either Message FileTree
    patchLogs = case logFile `HashMap.lookup` dContent vcs of
      Nothing -> return d
      Just ft -> do
        assertFile logFile ft
        return d
          { dVersion = Just . DirectoryVersion . lines $ fContent ft }

    patchRevs :: FileTree -> Either Message FileTree
    patchRevs dir = case revFolder `HashMap.lookup` dContent vcs of
      Nothing   -> return dir
      Just revs -> do
        assertDirectory revFolder revs
        HashMap.foldrWithKey (revRecurseStep []) (Right dir) $ dContent revs

    revRecurseStep
      :: [FilePath]
      -> FilePath
      -> FileTree
      -> Either Message FileTree
      -> Either Message FileTree
    revRecurseStep way name Directory{ dContent = cont } common = do
      realCommon <- common
      let bools = map (isDirectory . snd) . HashMap.toList $ cont
      if and bools && not (null bools)
      then HashMap.foldrWithKey (revRecurseStep $ way ++ [name]) common cont
      else
        if or bools
        then wrongVcsStructure
        else
          FileVersion <$> splitNumbers (HashMap.toList cont) >>=
          updateVersion (way ++ [name]) realCommon
    revRecurseStep _ _ _ _ = wrongVcsStructure

    splitNumbers :: [(FilePath, FileTree)] -> Either Message [Revision]
    splitNumbers list = assertSequence . sort $
      map (\(a, b) -> (splitOn ". " a, fContent b)) list

    assertSequence :: [([FilePath], String)] -> Either Message [Revision]
    assertSequence list =
      case sequence $ map (readMaybeInt . head . fst) list of
        Nothing      -> Left "wrong vcs revision format"
        Just intList ->
          if intList == [0..length intList - 1]
          then
            Right $ map (\(a, b) -> Revision (intercalate ". " $ tail a) b) list
          else Left "wrong vcs revision format: some numbers are missing"

    readMaybeInt :: String -> Maybe Int
    readMaybeInt = readMaybe

updateVersion
  :: [FilePath]
  -> FileTree
  -> FileVersion
  -> Either Message FileTree
updateVersion []    f@File{} fv = Right f{ fVersion = Just fv }
updateVersion (_:_)   File{} _  = Left  "incorrent vcs structure"
updateVersion [] Directory{} _  = Left  "vcs path not found"
updateVersion (x:xs) d@Directory{ dContent = dc } fv =
  case x `HashMap.lookup` dc of
    Nothing    -> Left "vcs path not found"
    Just found -> do
      updated <- updateVersion xs found fv
      return d { dContent = HashMap.insert x updated dc }
