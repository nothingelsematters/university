{-# LANGUAGE OverloadedStrings #-}

module Spec
  ( main
  ) where

import Control.Monad (void)
import Control.Monad.Except (runExcept)
import Control.Monad.State (get, put)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT (..), runStateT)
import qualified Data.HashMap.Lazy as HashMap (member, (!))
import Data.Maybe (fromJust)
import System.FilePath.Posix (joinPath, (</>))

import FileSystemShell.FileManager (cat, cd, fd, ls, mkFile, mkdir, rm, write)
import FileSystemShell.Types (Command, FileTree (..), MergeStrategy (..),
                              ShellState (..), emptyDirectory, isDirectory)
import FileSystemShell.VersionControlSystem (add, deleteVersion, history,
                                             initVcs, merge, showRevision,
                                             update)


main :: IO ()
main = either (putStrLn . ("ERROR: " ++)) (const $ putStrLn "success") .
  runStateT test $ ShellState (emptyDirectory "root") [] "/"


runTest
  :: Command
  -> Bool
  -> String
  -> StateT ShellState (Either String) (Maybe String)
runTest cmd res comment = do
  state <- get
  case runExcept $ runStateT cmd state of
    Left  _ ->
      if not res
      then return Nothing
      else lift . Left $ "must succeed: " ++ comment
    Right (msg, newState) ->
      if res
      then put newState >> return msg
      else lift . Left $ "must fail: " ++ comment

runSilently :: Command -> Bool -> String -> StateT ShellState (Either String) ()
runSilently cmd res = void . runTest cmd res

assert :: String -> Bool -> StateT ShellState (Either String) ()
assert _   True  = return ()
assert msg False = lift $ Left msg

test :: StateT ShellState (Either String) (Maybe String)
test = fileManagerTest >> vcsTest

fileManagerTest :: StateT ShellState (Either String) (Maybe String)
fileManagerTest = do
  -- ls
  runSilently (ls "..") False "ls: fail on .."
  msg <- runTest (ls ".") True  "ls: empty root"
  assert "ls: doesn't output anything on empty directory" $ msg == Just ""

  -- mkFile
  let fileName = "file"
  runSilently (mkFile fileName) True "mkFile"
  withFile <- currentFileTree <$> get
  assert "mkFile: creates file" $ fileName `HashMap.member` dContent withFile
  runSilently (mkFile fileName) False "mkFile again"

  -- mkdir
  runSilently (mkdir fileName) False "mkdir: on file"
  let dirName = "dir"
  runSilently (mkdir dirName) True "mkdir"
  withDir <- currentFileTree <$> get
  assert "mkdir: creates dir" . isDirectory $ dContent withDir HashMap.! dirName

  -- ls & mkFile again
  list <- runTest (ls ".") True  "ls"
  assert "ls" $ list == Just (unlines [dirName, fileName])

  -- write
  runSilently (write fileName "string") True "write"
  runSilently (cat fileName) True "cat"

  -- rm & ls
  runSilently (rm fileName) True "rm"
  rmList <- runTest (ls ".") True  "ls removed"
  assert "rm" $ rmList == Just (unlines [dirName])

  -- mkFile & fd
  runSilently (mkFile $ dirName </> fileName) True "mkFile"
  found <- runTest (fd fileName) True "fd"
  assert "fd" $
    found == Just (unlines . pure $ joinPath ["root", dirName, fileName])

  -- cd
  runSilently (cd fileName) False "false cd"
  runSilently (cd dirName) True "true cd"
  curDir <- currentFolder <$> get
  assert "cd" $ curDir == [dirName]
  runSilently (cd "..") True "cd upwards"
  runTest (cd "..") False "cd above the root"


vcsTest :: StateT ShellState (Either String) (Maybe String)
vcsTest = do
  let file = "file-name"
  runSilently initVcs True "vcs init vcs"
  runSilently (mkFile file) True "touch new"
  runSilently (add file) True "vcs add new"
  runSilently (update file "init") True "vcs update"
  runSilently (write file "new") True "write"
  runSilently (update file "second commit") True "vcs update second"
  runSilently (merge file 0 1 AcceptBoth) True "vcs merge"
  rev <- runTest (showRevision file 0) True "vcs show revision"
  assert "vcs show revision" $ rev == Just ""
  secondRev <- runTest (showRevision file 1) True "vcs show revision"
  merged <- runTest (showRevision file 2) True "vcs show revision"
  assert "merge logging" $
    fromJust merged == fromJust rev ++ "\n>>>>>>>\n" ++ fromJust secondRev
  runSilently (deleteVersion file 0) True "vcs delete revision"
  revs <- runTest (history file) True "vcs file history"
  assert "vcs file history after remove" $ (length . lines <$> revs) == Just 2
  return Nothing
