module Git where
  import Control.Applicative ((<$>))
  import qualified Data.Text as T
  import System.Process (readProcessWithExitCode)
  import System.Exit (ExitCode(ExitFailure, ExitSuccess))

  clone :: FilePath -> T.Text -> IO (Either String String)
  clone path url = parseResult <$> (readProcessWithExitCode "git" ["clone", (T.unpack url), path] "")

  parseResult :: (ExitCode, String, String) -> Either String String
  parseResult (ExitFailure _, _, err) = Left err
  parseResult (ExitSuccess, _, _)     = Right "Repo successfully cloned!"
