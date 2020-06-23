{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

---------------------------------------------------------------------
-- |
-- Module      : FileSystemShell.Types
-- Description : Basic types
-- Copyright   : (c) Simon Naumov, 2020
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
--
-- Describes common types to use in this file system implementation.
---------------------------------------------------------------------

module FileSystemShell.Types
  ( -- * File system
    FileTree(..)
  , emptyDirectory
  , emptyFile
  , getNodeName
  , isDirectory
  , isFile
  , showProperties
  , treeFileAmount
  , treeSize

   -- * Version control system
  , DirectoryVersion (..)
  , FileVersion (..)
  , MergeStrategy (..)
  , Revision (..)

   -- * Shell
  ,  Action
  , Command
  , Message
  , Shell(..)
  , ShellState(..)
  ) where

import Control.Applicative (Alternative, (<|>))
import Control.Monad.Except (Except)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.State (StateT)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap (empty, toList)
import Data.List (isPrefixOf)
import Data.Text (pack, replace, unpack)
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..))
import System.Directory (Permissions (..), emptyPermissions, setOwnerReadable,
                         setOwnerSearchable, setOwnerWritable)
import System.FilePath.Posix (takeExtension)


-- | File tree implementation: it can be a directory or a file.
data FileTree
  = Directory
    { dContent     :: HashMap FilePath FileTree -- ^ content: files and folders
    , dVersion     :: Maybe DirectoryVersion    -- ^ vcs logs
    , dName        :: FilePath                  -- ^ name
    , dSize        :: Integer                   -- ^ recursive size
    , dFileAmount  :: Integer                   -- ^ recursive file amount
    , dPermissions :: Permissions               -- ^ permissions
    }
  | File
    { fContent          :: String            -- ^ content
    , fVersion          :: Maybe FileVersion -- ^ revisions
    , fName             :: FilePath          -- ^ name
    , fPermissions      :: Permissions       -- ^ permissions
    , fType             :: String            -- ^ type (extension)
    , fModificationTime :: UTCTime           -- ^ last modification time
    , fSize             :: Integer           -- ^ size in bytes
    } deriving Eq


instance Show FileTree where
  show = showIndented ""
    where
      showIndented :: String -> FileTree -> String
      showIndented ind Directory{ dContent = c, dName = dn } = do
        let replaces  = replace "└── " "    " . replace "├── " "│   "
        let newIndent = unpack . replaces . pack $ ind
        ind ++ dn ++ "\n" ++ showListIndented newIndent
          (map snd $ HashMap.toList c)
      showIndented ind File{ fName = fn } = ind ++ fn ++ "\n"

      showListIndented :: String -> [FileTree] -> String
      showListIndented ind list =
        if null list
        then ""
        else
          concat (map (showIndented (ind ++ "├── ")) (tail list))
          ++ showIndented (ind ++ "└── ") (head list)

-- | Show file or directory permissions.
showPermissions :: Permissions -> String
showPermissions perms = map (uncurry showCharIf)
    [ ('r', readable)
    , ('w', writable)
    , ('e', executable)
    , ('s', searchable)
    ]
  where
    showCharIf :: Char -> (Permissions -> Bool) -> Char
    showCharIf char boolFunc =
      if boolFunc perms
      then char
      else '-'

-- | Outputs file tree size.
treeSize :: FileTree -> Integer
treeSize      File{ fSize = fs } = fs
treeSize Directory{ dSize = ds } = ds

-- | Outputs a file system amount.
treeFileAmount :: FileTree -> Integer
treeFileAmount File{}                         = 1
treeFileAmount Directory{ dFileAmount = dfa } = dfa

-- | Outputs a file syste node name.
getNodeName :: FileTree -> FilePath
getNodeName      File{ fName = fn } = fn
getNodeName Directory{ dName = dn } = dn

-- | Show file or directory properties.
showProperties :: FileTree -> String
showProperties f@File{} = unwords
  [ showPermissions $ fPermissions f
  , show $ fModificationTime f
  , fType f
  , show $ fSize f
  , "bytes"
  , fName f
  ]
showProperties d@Directory{} = unwords
  [ showPermissions $ dPermissions d
  , show $ dFileAmount d
  , "files"
  , show $ dSize d
  , "bytes"
  , dName d
  ]

-- | An empty file. __Very important to mention__: because of file system
-- implemented with 'FileTree' is not really on disk at the moment of
-- processing, so creating a file is not really creating a file system instance
-- on a disk, so this file is out of space and time and it causes it to have
-- zero time set in 'fModificationTime' field.
emptyFile :: String -> FileTree
emptyFile name = File
  { fContent          = ""
  , fVersion          = Nothing
  , fName             = name
  , fPermissions      = perms
  , fType             = takeExtension name
  , fModificationTime = zeroTime
  , fSize             = 0
  }
  where
    perms :: Permissions
    perms = setOwnerReadable True . setOwnerWritable True $ emptyPermissions

-- | An empty directory.
emptyDirectory :: String -> FileTree
emptyDirectory name = Directory
  { dContent     = HashMap.empty
  , dVersion     = Nothing
  , dName        = name
  , dSize        = 0
  , dFileAmount  = 0
  , dPermissions = perms
  }
  where
    perms :: Permissions
    perms = setOwnerReadable True
      . setOwnerWritable True
      . setOwnerSearchable True
      $ emptyPermissions

-- | Checks, whether a given node is a directory.
isDirectory :: FileTree -> Bool
isDirectory Directory{} = True
isDirectory _           = False

-- | Checks, whether a given node is a file.
isFile :: FileTree -> Bool
isFile File{} = True
isFile _      = False

-- | Zero time.
zeroTime :: UTCTime
zeroTime = UTCTime (ModifiedJulianDay 0) 0

-- | File version history for version control system, that basically consists
-- of a list of revisions
newtype FileVersion      = FileVersion [Revision] deriving Eq

-- | Directory version history for version control system, that basically
-- consists of a list of changes
newtype DirectoryVersion = DirectoryVersion [String] deriving Eq

-- | A revision data class, that consists basically of a commit message and file
-- content.
data Revision
  = Revision
  { rMessage :: String -- ^ commit message
  , rContent :: String -- ^ file contents
  } deriving Eq

-- | A merge strategy that can be used in
-- 'FileSystemShell.VersionControlSystem.merge' command.
-- The following merge strategies are available:
--
--     * "left": chooses first revision over second
--
--     * "second": chooses second revision over first
--
--     * "both": concatenates both revisions
--
--     * "interactive": git-like diff merge
data MergeStrategy
  = AcceptLeft  -- ^ chooses first revision
  | AcceptRight -- ^ chooses second revision
  | AcceptBoth  -- ^ concatenates revisions
  | Interactive -- ^ leaves user to manage diffs

instance Read MergeStrategy where
  readsPrec _ input = check "left" AcceptLeft
    <|> check "right"       AcceptRight
    <|> check "both"        AcceptBoth
    <|> check "interactive" Interactive
    where
      check :: String -> MergeStrategy -> [(MergeStrategy, String)]
      check str res =
        if str `isPrefixOf` input
        then [(res, drop (length str) str)]
        else []

-- | Interactive shell type - it is a 'StateT' over 'ShellState' and 'IO'
newtype Shell a = Shell { unShell :: StateT ShellState IO a }
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadState ShellState
    , MonadIO
    , MonadFail
    )

-- | Interactive shell type: it consists of current 'FileTree' and current
-- directory, in which the user is operation at the moment.
data ShellState = ShellState
  { currentFileTree :: FileTree    -- ^ current file system state
  , currentFolder   :: [FilePath]  -- ^ current file system folder
  , globalPath      :: FilePath    -- ^ global path of an indexed directory
  }

-- | A 'String' alias, that tells, that this string is used for printing out.
type Message = String

-- | An interactive shell action to be performed.
type Action a = StateT ShellState (Except String) a

-- | An interactive shell common command. 'Action' with info message to output.
type Command = Action (Maybe Message)
