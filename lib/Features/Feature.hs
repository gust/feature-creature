module Features.Feature where
  import Data.List (stripPrefix)
  import Data.Maybe (mapMaybe)
  import System.Exit (ExitCode(ExitFailure, ExitSuccess))
  import System.Process (readProcessWithExitCode)

  type FeatureFile = FilePath

  showFeatures :: FilePath -> IO [FeatureFile]
  showFeatures path = do
    result <- readProcessWithExitCode "find" [path, "-type", "f", "-name", "*.feature"] ""
    case result of
      (ExitFailure _, stdout, stderr) -> return $ lines (stderr ++ stdout)
      (ExitSuccess, stdout, stderr) -> return $ mapMaybe stripPath $ lines (stderr ++ stdout)
        where
          stripPath = (stripPrefix path)
