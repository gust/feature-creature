module Git where
  import Control.Applicative ((<$>))
  import qualified Data.Text as T
  import System.Process (readProcessWithExitCode)
  import System.Exit (ExitCode(ExitFailure, ExitSuccess))

  clone :: FilePath -> T.Text -> IO (Either String String)
  clone path url = parseResult' <$> cloneCmd'
    where
      parseResult' :: (ExitCode, String, String) -> Either String String
      parseResult' = parseResult "Repo successfully cloned!"

      cloneCmd' :: IO (ExitCode, String, String)
      cloneCmd' = readProcessWithExitCode "git" ["clone", (T.unpack url), path] ""

  pull :: FilePath -> IO (Either String String)
  pull path = parseResult' <$> pullCmd'
    where
      parseResult' :: (ExitCode, String, String) -> Either String String
      parseResult' = parseResult "Repo successfully updated!"

      pullCmd' :: IO (ExitCode, String, String)
      pullCmd' = readProcessWithExitCode "git" ["-C", path, "pull"] ""

  parseResult :: String -> (ExitCode, String, String) -> Either String String
  parseResult _ (ExitFailure _, _, err) = Left err
  parseResult successMsg (ExitSuccess, _, _)     = Right successMsg
