module Git
( clone
, diff
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

diff :: FilePath -> WithErr String
diff path = do
  result <- liftIO $ gitRepoCommand path ["diff", "--name-status"]
  parseResult result

parseResult :: (ExitCode, String, String) -> WithErr String
parseResult (ExitFailure _, stdout, stderr) = throwError $ stderr ++ stdout
parseResult (ExitSuccess, stdout, stderr)   = return $ stderr ++ stdout

gitRepoCommand :: FilePath -> [String] -> IO (ExitCode, String, String)
gitRepoCommand path args = gitCommand $ defaults <> args
  where
    defaults = ["-C", path]

gitCommand :: [String] -> IO (ExitCode, String, String)
gitCommand args = readProcessWithExitCode "git" args ""
