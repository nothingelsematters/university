-------------------------------------------------------------
-- |
-- Module      : FileSystemShell.CommandParser
-- Description : Command parser
-- Copyright   : (c) Simon Naumov, 2020
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
--
-- This module parses the user's input in interactive shell.
-------------------------------------------------------------

module FileSystemShell.CommandParser
  ( parseCommand
  ) where

import Data.Tuple.Extra (fst3)
import Options.Applicative (CommandFields, Mod, Parser, ParserFailure (..),
                            ParserInfo, ParserResult (..), argument, auto,
                            command, commandGroup, defaultPrefs, execParserPure,
                            fullDesc, header, help, helper, hsubparser, idm,
                            info, infoOption, metavar, progDesc, showDefault,
                            strArgument, value, (<**>), (<|>))

import FileSystemShell.FileManager (cat, cd, fd, ls, mkFile, mkdir, nodeInfo,
                                    rm, tree, write)
import FileSystemShell.Types (Command)
import FileSystemShell.VersionControlSystem (add, deleteVersion, diff, history,
                                             initVcs, logVcs, merge, remove,
                                             showRevision, update)


-- | Given the string to parse it returns a parse error or 'Command' to that
-- was asked to execute. More about commands: 'FileSystemShell.FileManager'
-- and 'FileSystemShell.VersionControlSystem'.
parseCommand :: String -> Either String Command
parseCommand = handle . execParserPure defaultPrefs opts . words
  where
    handle :: ParserResult a -> Either String a
    handle (Success s) = Right s
    handle (Failure (ParserFailure f)) = Left . show . fst3 $ f ""
    handle (CompletionInvoked _) = Left "completions are not supported yet"


opts :: ParserInfo Command
opts = info (commandStringParser <**> infoOption "qwe" idm <**> helper)
  (  fullDesc
  <> progDesc "a simple shell-like file manager and version control system"
  <> header "sfm - simple file manager"
  )

directoryArg :: (FilePath -> a) -> Parser a
directoryArg = (<$> strParser "DIRECTORY")

fileArg :: (FilePath -> a) -> Parser a
fileArg = (<$> strParser "FILE")

pathArg :: (FilePath -> a) -> Parser a
pathArg = (<$> strParser "FILE/DIRECTORY")

namedArg :: Read a => String -> Parser a
namedArg = argument auto . metavar

strParser :: String -> Parser String
strParser = strArgument . metavar

cmd :: String -> Parser a -> String -> Mod CommandFields a
cmd name parser desc = command name . info parser $ progDesc desc

commandStringParser :: Parser Command
commandStringParser = fileManagerParser
  <|> hsubparser
  (  commandGroup "Version Control System:"
  <> cmd "vcs" vcsParser "version control system"
  )


fileManagerParser :: Parser Command
fileManagerParser = hsubparser
  (  commandGroup "File Managing:"
  <> cmd "cd"     (directoryArg cd)      "change directory"
  <> cmd "ls"     (dottedDirectory ls)   "list directory contents"
  <> cmd "tree"   (dottedDirectory tree) "output directory recursively"
  <> cmd "mkdir"  (directoryArg mkdir)   "create new folder"
  <> cmd "cat"    (fileArg cat)          "output file content"
  <> cmd "mkFile" (fileArg mkFile)       "create file or update modification time"
  <> cmd "rm"     (pathArg rm)           "remove file or directory"
  <> cmd "write"   writeParser           "write string into a file"
  <> cmd "fd"     (fileArg fd)           "find file or directory recursively"
  <> cmd "info"   (pathArg nodeInfo)     "print file or directory info"
  )
  where
    writeParser :: Parser Command
    writeParser = fileArg write <*> strParser "CONTENT"

    dottedDirectory :: (FilePath -> Command) -> Parser Command
    dottedDirectory = flip fmap $ strArgument
      ( metavar "DIRECTORY"
      <> showDefault
      <> value "."
        )


vcsParser :: Parser Command
vcsParser = hsubparser
  (  commandGroup "Information commands:"
  <> cmd "log"          (pure logVcs)          "show vcs history"
  <> cmd "file-history" (fileArg history)      "show file vcs history"
  <> cmd "show"         (revArg showRevision)  "show file revision"
  <> cmd "diff"         (twoRevsArg diff)      "show revisions diff"
  )
  <|> hsubparser
  (  commandGroup "Editing commands:"
  <> cmd "init"         (pure initVcs)         "initiate vcs"
  <> cmd "add"          (pathArg add)          "add file/folder to vcs"
  <> cmd "update"        updateParser          "add file changes to vcs"
  <> cmd "merge"         mergeParser           "merge file revisions"
  <> cmd "remove"       (pathArg remove)       "remove file from vcs"
  <> cmd "rm-rev"       (revArg deleteVersion) "remove file revision"
  )
  where
    revArg :: (FilePath -> Int -> a) -> Parser a
    revArg = (<*> namedArg "REVISION") . fileArg

    twoRevsArg :: (FilePath -> Int -> Int -> a) -> Parser a
    twoRevsArg = (<*> namedArg "SECOND-REVISION") . revArg

    updateParser :: Parser Command
    updateParser = fileArg update <*> strParser "COMMENT"

    mergeParser :: Parser Command
    mergeParser = twoRevsArg merge <*> argument auto
      (  metavar "STRATEGY"
      <> help "merge strategy, one of: left | right | both | interactive"
      )
