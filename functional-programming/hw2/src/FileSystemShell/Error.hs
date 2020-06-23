--------------------------------------------------------------------------
-- |
-- Module      : FileSystemShell.Error
-- Description : Error messages and handling
-- Copyright   : (c) Simon Naumov, 2020
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
--
-- This modules contains common error that often appears while performing
-- different actions with file system.
--------------------------------------------------------------------------

module FileSystemShell.Error
  ( directoryExpected
  , fileExpected
  , liftThrow
  , notFound
  , notInVcs
  , notReadable
  , notWritable
  , wrongVcsStructure
  ) where

import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT, throwE)

import FileSystemShell.Types (Action)


-- | A helper function that transforms a string into an exception case in
-- 'ExceptT'.
liftThrow :: (MonadTrans t, Monad m) => e -> t (ExceptT e m) a
liftThrow = lift . throwE

-- | Path not found exception.
notFound :: Action a
notFound = liftThrow "path not found"

-- | An exception raised while being asked to perform a file action, but the
-- path given turns out to be a ditectory.
fileExpected :: Action a
fileExpected = liftThrow "directory found, while file was expected"

-- | An exception raised while being asked to perform a directory action, but
-- the path given turns out to be a file.
directoryExpected :: Action a
directoryExpected = liftThrow "file found, while directory was expected"

-- | Wrong version control system structure exception.
wrongVcsStructure :: Either String a
wrongVcsStructure = Left "vcs directory has wrong structure"

-- | An excepton raised while trying to perform an action on a file, that is
-- supposed to be in version control system, but turns out not to be.
notInVcs :: Action a
notInVcs = liftThrow "file not in vcs"

-- | An error message, that is raised when a node is not readable.
notReadable :: Action a
notReadable = liftThrow "you have no read permissions"

-- | An error message, that is raised when a node is not writable.
notWritable :: Action a
notWritable = liftThrow "you have no write permissions"
