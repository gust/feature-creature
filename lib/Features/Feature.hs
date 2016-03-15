{-# LANGUAGE OverloadedStrings #-}

module Features.Feature
( Feature (..)
, FeatureFile (..)
, buildDirectoryTree
, findFeatureFiles
, getFeature
, getFeatures
) where

import CommonCreatures (WithErr)
import Data.DirectoryTree (DirectoryTree, FileDescription (..), createNode, addToDirectoryTree)
import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)

newtype Feature     = Feature String deriving (Show)
newtype FeatureFile = FeatureFile String deriving (Show)

getFeatures :: FilePath -> WithErr DirectoryTree
getFeatures path = buildDirectoryTree <$> findFeatureFiles path

getFeature :: FeatureFile -> WithErr Feature
getFeature (FeatureFile path) = do
  fileExists <- liftIO $ doesFileExist path
  case fileExists of
    False -> throwError $ "Feature file does not exist at path: " ++ path
    True  -> do
      fileContents <- liftIO (readFile path)
      return $ Feature fileContents

buildDirectoryTree :: [FeatureFile] -> DirectoryTree
buildDirectoryTree =
  foldr (\(FeatureFile featureFile) dirTree -> addToDirectoryTree dirTree featureFile) rootNode
  where
    rootNode :: DirectoryTree
    rootNode = createNode $ FileDescription "/" "/"

findFeatureFiles :: FilePath -> WithErr [FeatureFile]
findFeatureFiles path = do
  result <- liftIO $ readProcessWithExitCode "find" [path, "-type", "f", "-name", "*.feature"] ""
  parseFeatureFiles result path

parseFeatureFiles :: (ExitCode, String, String) -> FilePath -> WithErr [FeatureFile]
parseFeatureFiles (ExitFailure _, stdout, stderr) _      = throwError $ stderr ++ stdout
parseFeatureFiles (ExitSuccess, stdout, stderr) basePath = return $ FeatureFile <$> (mapMaybe stripPath $ outputLines)
  where
    stripPath = (stripPrefix basePath)
    outputLines = lines (stderr ++ stdout)
