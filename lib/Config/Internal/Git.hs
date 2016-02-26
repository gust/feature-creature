module Config.Internal.Git
( GitConfig (..)
, readGitConfig
) where

import System.Environment (getEnv)

data GitConfig =
  GitConfig { repoBasePath :: String
            } deriving (Show)

readGitConfig :: IO GitConfig
readGitConfig =
  GitConfig
  <$> getEnv "FC_DATA_FILES_PATH"
