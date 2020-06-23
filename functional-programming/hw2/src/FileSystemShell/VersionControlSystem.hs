{-# LANGUAGE LambdaCase #-}

---------------------------------------------------------------------------
-- |
-- Module      : FileSystemShell.VersionControlSystem
-- Description : Version control system
-- Copyright   : (c) Simon Naumov, 2020
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
--
-- This module consists of common useful version control system functions.
---------------------------------------------------------------------------

module FileSystemShell.VersionControlSystem
  ( -- * Info commands
    diff
  , history
  , logFile
  , showRevision

   -- * Editing commands
  , add
  , deleteVersion
  , initVcs
  , merge
  , remove
  , update

   -- * Metadata path names
  , logVcs
  , revFolder
  , vcsFolder
  ) where

import Control.Monad (void)
import Control.Monad.State (get)
import Data.Algorithm.Diff (Diff, PolyDiff (..), getDiff)
import qualified Data.HashMap.Lazy as HashMap (lookup)

import FileSystemShell.Error (directoryExpected, liftThrow, notFound, notInVcs)
import FileSystemShell.FileManager (write)
import FileSystemShell.Types (Action, Command, DirectoryVersion (..),
                              FileTree (..), FileVersion (..),
                              MergeStrategy (..), Message, Revision (..),
                              ShellState (..))
import FileSystemShell.Utils (assertReadable, assertWritable, findFile,
                              getCurrentDirectory, getRevisions, noMsg,
                              resolvePath, updateVcs, validateRevision)


-- | Version control system folder name, where to store metadata on dumping to
-- and on reading from.
vcsFolder :: FilePath
vcsFolder = ".vcs"

-- | A metadata file name, where to store whole repository log.
logFile :: FilePath
logFile = "log"

-- | A metadata directory name, where to store file revisions.
revFolder :: FilePath
revFolder = "rev"

-- | Initiates version control system in current directory.
initVcs :: Command
initVcs = do
  getCurrentDirectory >>= assertWritable >>= assertNoVcsSubdirectory
  updateVcs "vcs initiated" initiate "."
  where
    initiate :: FileTree -> Action FileTree
    initiate File{} = directoryExpected
    initiate Directory{ dVersion = Just _ } = liftThrow "vcs already inited"
    initiate d@Directory{} = return d{ dVersion = Just $ DirectoryVersion [] }

    assertNoVcsSubdirectory :: FileTree -> Action ()
    assertNoVcsSubdirectory File{} = return ()
    assertNoVcsSubdirectory Directory{ dVersion = Just _ } =
      liftThrow "vcs already initiated in this directory or its subdirectories"
    assertNoVcsSubdirectory Directory{ dContent = cont } =
      mapM_ assertNoVcsSubdirectory cont

-- | Adds a given path into the version control system. Fails if one of the path
-- or subpaths is in vcs.
add :: FilePath -> Command
add = updateVcs "added" recursiveAdd
  where
    recursiveAdd :: FileTree -> Action FileTree
    recursiveAdd File{ fVersion = Just _ } = liftThrow "file already in vcs"
    recursiveAdd f@File{} = return f{ fVersion = Just (FileVersion []) }
    recursiveAdd d@Directory{ dContent = dc } = do
      newContent <- mapM recursiveAdd dc
      return d{ dContent = newContent }

-- | Shows log of a repository of a current directory.
logVcs :: Command
logVcs = do
  state <- get
  way <- resolvePath "."
  loggerDive way $ currentFileTree state
  where
    loggerDive :: [FilePath] -> FileTree -> Command
    loggerDive _ Directory{ dVersion = Just (DirectoryVersion dv) } =
      return . Just $ unlines dv
    loggerDive (x:xs) Directory{ dContent = dc } =
      case x `HashMap.lookup` dc of
        Nothing  -> notFound
        Just dir -> loggerDive xs dir
    loggerDive _ _ = notInVcs

-- | Shows a certain file revision history.
history :: FilePath -> Command
history path = findFile path >>= getRevisions >>=
  return . pure . unlines . map (uncurry (++))
  . zip (map ((++ ". ") . show) [(0 :: Int)..]) . map rMessage

-- | Updates file state in version control system.
update :: FilePath -> String -> Command
update path msg = updateVcs (msg ++ ": updated") updateFile path
  where
    updateFile :: FileTree -> Action FileTree
    updateFile f = do
      void $ assertReadable f
      newRevs <- (++ [Revision msg $ fContent f]) <$> getRevisions f
      return f{ fVersion = Just $ FileVersion newRevs }

-- | Shows a certain file revision.
showRevision :: FilePath -> Int -> Command
showRevision path int = do
  found <- findFile path
  pure . rContent <$> validateRevision found int

-- | Merge two file revisions. The following merge strategies are available:
--
--     * "left": chooses first revision over second
--
--     * "second": chooses second revision over first
--
--     * "both": concatenates both revisions
--
--     * "interactive": git-like diff merge
--
merge :: FilePath -> Int -> Int -> MergeStrategy -> Command
merge _ first second _ | first == second = liftThrow "merging same revs?"
merge path first second AcceptLeft  = chooseRev path first second
merge path first second AcceptRight = chooseRev path second first
merge path first second AcceptBoth  = do
  f <- findFile path
  firstRev  <- validateRevision f first
  secondRev <- validateRevision f second
  writeAndUpdate path
    (rContent firstRev ++ "\n>>>>>>>\n" ++ rContent secondRev)
    first second
merge path first second Interactive = do
  diffs <- getRevisionDiff path first second
  void $ write path diffs
  return . pure $
    "diff of " ++ show first ++ " | " ++ show second ++ " commits "
    ++ "has been written to " ++ path
    ++ "\nyou can either:"
    ++ "\n    + use `write` command and resolve conflicts "
    ++ "(if there are any) and `vcs update` then"
    ++ "\n    + `exit`, use your favourite file editor, then launch again "
    ++ "and `vcs update`"
    ++ "\n\nchoose your side"

chooseRev :: FilePath -> Int -> Int -> Command
chooseRev path chosen over = do
  found <- findFile path
  chosenRev <- validateRevision found chosen
  void $ validateRevision found over
  writeAndUpdate path (rContent chosenRev) chosen over

writeAndUpdate :: FilePath -> String -> Int -> Int -> Command
writeAndUpdate path content first second = noMsg $ do
  void $ write path content
  void . update path $ "merged " ++ show first ++ " with " ++ show second

-- | Deletes certain file revision.
deleteVersion :: FilePath -> Int -> Command
deleteVersion path rev =
  updateVcs ("deleted " ++ show rev ++ "revision ") delete path
  where
    delete :: FileTree -> Action FileTree
    delete f = do
      revs <- getRevisions f
      void $ validateRevision f rev
      return f
        { fVersion =
            Just (FileVersion $ take rev revs ++ drop (rev + 1) revs)
        }

-- | Removes file out of version control system.
remove :: FilePath -> Command
remove = updateVcs "removed" $ \f ->
  const f{ fVersion = Nothing } <$> getRevisions f

-- | Calculates revision diff.
diff :: FilePath -> Int -> Int -> Command
diff path first = fmap pure . getRevisionDiff path first

getRevisionDiff :: FilePath -> Int -> Int -> Action String
getRevisionDiff path first second = do
  found <- findFile path
  let getRev = fmap (lines . rContent) . validateRevision found
  firstRev  <- getRev first
  secondRev <- getRev second
  return . showDiff . squash $ getDiff firstRev secondRev
    where
      showDiff :: [Diff String] -> Message
      showDiff = (unlines .) . map $ \case
        First  a -> show first  ++ " >>>>>>>>>>>>>\n" ++ a ++ "\n>>>>>>>>>>>>>"
        Second a -> show second ++ " <<<<<<<<<<<<<\n" ++ a ++ "\n<<<<<<<<<<<<<"
        Both a _ -> a

      squash :: [Diff String] -> [Diff String]
      squash (First  a : First  b : xs) = squash $ First  (a ++ "\n" ++ b) : xs
      squash (Second a : Second b : xs) = squash $ Second (a ++ "\n" ++ b) : xs
      squash (Both a _ : Both b _ : xs) = squash $
        Both (a ++ "\n" ++ b) (a ++ "\n" ++ b) : xs
      squash xs = xs
