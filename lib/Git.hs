module Git where
  import Control.Applicative ((<$>))
  import qualified Data.Text as T
  import System.Process (readProcessWithExitCode)
  import System.Exit (ExitCode(ExitFailure, ExitSuccess))

  clone :: FilePath -> T.Text -> IO (Either String String)
  clone path url = parseResult' <$> (readProcessWithExitCode "git" ["clone", (T.unpack url), path] "")
    where
      parseResult' = parseResult "Repo successfully cloned!"

  pull :: FilePath -> IO (Either String String)
  pull path = parseResult' <$> (readProcessWithExitCode "git" ["-C", path, "pull"] "")
    where
      parseResult' = parseResult "Repo successfully updated!"

  parseResult :: String -> (ExitCode, String, String) -> Either String String
  parseResult _ (ExitFailure _, _, err) = Left err
  parseResult successMsg (ExitSuccess, _, _)     = Right successMsg
