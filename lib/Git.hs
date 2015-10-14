module Git 
  ( clone
  , pull
  ) where

  import Control.Monad.Except (ExceptT, throwError)
  import Control.Monad.IO.Class (liftIO)
  import qualified Data.Text as T
  import System.Exit (ExitCode(ExitFailure, ExitSuccess))
  import System.Process (readProcessWithExitCode)

  type GitExceptT = ExceptT String IO

  clone :: FilePath -> T.Text -> GitExceptT String
  clone path url = do
    result <- liftIO $ readProcessWithExitCode "git" ["clone", (T.unpack url), path] ""
    parseResult result

  pull :: FilePath -> GitExceptT String
  pull path = do
    result <- liftIO $ readProcessWithExitCode "git" ["-C", path, "pull"] ""
    parseResult result

  parseResult :: (ExitCode, String, String) -> GitExceptT String
  parseResult (ExitFailure _, stdout, stderr) = throwError $ stderr ++ stdout
  parseResult (ExitSuccess, stdout, stderr)   = return $ stderr ++ stdout
