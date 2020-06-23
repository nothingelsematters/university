------------------------------------------------------------------------
-- |
-- Module      : FileSystemShell.Shell
-- Description : Interactive shell
-- Copyright   : (c) Simon Naumov, 2020
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
--
-- An interactive shell module. It describes only 'shellLoop' function.
------------------------------------------------------------------------

module FileSystemShell.Shell
  ( shellLoop
  ) where

import Control.Applicative (many)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
import Control.Monad.Trans.Except (runExcept)
import Control.Monad.Trans.State (runStateT)
import System.FilePath.Posix (joinPath)
import System.IO (hFlush, stdout)

import FileSystemShell.CommandParser (parseCommand)
import FileSystemShell.Types (FileTree, Shell (..), ShellState (..))


-- | This command launches an interactive loop to communicate with user and
-- execute asked commands.
shellLoop :: Shell [FileTree]
shellLoop = many $ do
  state <- get
  let constPrefix = globalPath state ++ " # "
  liftIO . putStr $ constPrefix ++ (joinPath $ currentFolder state) ++ " λ "
  liftIO $ hFlush stdout
  cmd <- liftIO getLine
  processSpecialCommand $ words cmd

processSpecialCommand :: [String] -> Shell FileTree
processSpecialCommand ["exit"] = fail "exiting"
processSpecialCommand ["help"] = processCommand "--help"
processSpecialCommand command  = processCommand $ unwords command

processCommand :: String -> Shell FileTree
processCommand cmd = do
  state <- get
  let processed = parseCommand cmd >>= \x -> runExcept $ runStateT x state
  let (msg, newState) = either (flip (,) state . pure) id processed
  maybe (return ()) (liftIO . putStrLn) msg
  put newState
  return $ currentFileTree newState
