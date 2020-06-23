module Main
  ( main
  ) where

import Control.Exception (IOException, catch, displayException)
import Control.Monad.Trans.State (execStateT)
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, header,
                            help, helper, info, metavar, progDesc, showDefault,
                            strArgument, value, (<**>))
import System.Directory (canonicalizePath, doesDirectoryExist)

import FileSystemShell.Scan (scan)
import FileSystemShell.Shell (shellLoop)
import FileSystemShell.Types (Shell (..), ShellState (..))
import FileSystemShell.Update (dumpState)


main :: IO ()
main = execParser opts >>= execute

execute :: FilePath -> IO ()
execute path = do
  canonicalPath <- canonicalizePath path
  existence <- doesDirectoryExist canonicalPath
  if not existence
  then putStrLn $ path ++ ": given directory does not exist"
  else do
    putStrLn "loading..."
    let showE = fmap ("ERROR: " ++) (displayException :: IOException -> String)
    scannerResult <- scan canonicalPath `catch` (return . Left . showE)
    case scannerResult of
      Left  msg     -> putStrLn msg
      Right scanned -> do
        print scanned
        let mkState ft = ShellState ft [] canonicalPath
        newState <- execStateT (unShell shellLoop) . mkState $ scanned
        dumpState newState `catch` (putStrLn . showE)

opts :: ParserInfo FilePath
opts = info (pathParser <**> helper)
  (  fullDesc
  <> progDesc "a simple shell-like file manager and version control system"
  <> header "fss - simple file system shell"
  )

pathParser :: Parser FilePath
pathParser = strArgument
  (  metavar "DIRECTORY"
  <> value "."
  <> showDefault
  <> help "directory where to launch this tool"
  )
