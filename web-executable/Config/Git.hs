module Config.Git
( GitConfig (..)
, getGitConfig
) where

import System.Environment (getEnv)

data GitConfig =
  GitConfig { repoBasePath :: String
            } deriving (Show)

getGitConfig :: IO GitConfig
getGitConfig =
  GitConfig
  <$> getEnv "FC_DATA_FILES_PATH"
