module Config.Internal.Git
( GitConfig (..)
, readGitConfig
) where

import System.Environment (getEnv, lookupEnv)

data GitConfig =
  GitConfig { repoBasePath :: String
            , refreshInterval :: Int
            } deriving (Show)

readGitConfig :: IO GitConfig
readGitConfig =
  GitConfig
  <$> getEnv "FC_DATA_FILES_PATH"
  <*> ((maybe (60 :: Int) read) <$> lookupEnv "FC_REPO_REFRESH_SECONDS")
