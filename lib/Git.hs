module Git
( FileModification (..)
, clone
, statusDiff
, parseStatusDiff
, fetch
, pull
) where

import CommonCreatures (WithErr)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import qualified Data.Text as T
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)

import Text.ParserCombinators.Parsec
import Control.Monad (void)

data FileModification = Added FilePath
                      | Copied FilePath
                      | Deleted FilePath
                      | Modified FilePath
                      | Renamed FilePath
                      | TypeChanged FilePath
                      | Unmerged FilePath
                      | Unknown FilePath
                      | Broken FilePath
                      | Unrecognized Char FilePath
  deriving (Show, Eq)

clone :: FilePath -> T.Text -> WithErr String
clone path url = do
  result <- liftIO $ gitCommand ["clone", (T.unpack url), path]
  parseResult result

pull :: FilePath -> WithErr String
pull path = do
  result <- liftIO $ gitRepoCommand path ["pull"]
  parseResult result

fetch :: FilePath -> WithErr String
fetch path = do
  result <- liftIO $ gitRepoCommand path ["fetch"]
  parseResult result

statusDiff :: FilePath -> WithErr String
statusDiff path = do
  result <- liftIO $ gitRepoCommand path ["diff", "origin", "--name-status"]
  parseResult result

parseStatusDiff :: [String] -> [Either ParseError FileModification]
parseStatusDiff = map parseStatusDiff'

parseStatusDiff' :: String -> Either ParseError FileModification
parseStatusDiff' d = parse parseFileModification "fileModification" d

parseFileModification :: Parser FileModification
parseFileModification = do
  modType <- oneOf "ACDMRTUXB"
  void $ many1 $ char ' '
  fileName <- many1 $ anyChar
  return $ fileModification modType fileName

fileModification :: Char -> String -> FileModification
fileModification 'A' fileName = Added fileName
fileModification 'C' fileName = Copied fileName
fileModification 'D' fileName = Deleted fileName
fileModification 'M' fileName = Modified fileName
fileModification 'R' fileName = Renamed fileName
fileModification 'T' fileName = TypeChanged fileName
fileModification 'U' fileName = Unmerged fileName
fileModification 'X' fileName = Unknown fileName
fileModification 'B' fileName = Broken fileName
fileModification abbr fileName = Unrecognized abbr fileName

parseResult :: (ExitCode, String, String) -> WithErr String
parseResult (ExitFailure _, stdout, stderr) = throwError $ stderr ++ stdout
parseResult (ExitSuccess, stdout, stderr)   = return $ stderr ++ stdout

gitRepoCommand :: FilePath -> [String] -> IO (ExitCode, String, String)
gitRepoCommand path args = gitCommand $ defaults <> args
  where
    defaults = ["-C", path]

gitCommand :: [String] -> IO (ExitCode, String, String)
gitCommand args = readProcessWithExitCode "git" args ""
