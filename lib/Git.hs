module Git
( clone
, fetch
, pull
) where

import CommonCreatures (WithErr)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)

clone :: FilePath -> T.Text -> WithErr String
clone path url = do
  result <- liftIO $ readProcessWithExitCode "git" ["clone", (T.unpack url), path] ""
  parseResult result

pull :: FilePath -> WithErr String
pull path = do
  result <- liftIO $ readProcessWithExitCode "git" ["-C", path, "pull"] ""
  parseResult result

fetch :: FilePath -> WithErr String
fetch path = do
  result <- liftIO $ readProcessWithExitCode "git" ["-C", path, "fetch"] ""
  parseResult result

parseResult :: (ExitCode, String, String) -> WithErr String
parseResult (ExitFailure _, stdout, stderr) = throwError $ stderr ++ stdout
parseResult (ExitSuccess, stdout, stderr)   = return $ stderr ++ stdout
