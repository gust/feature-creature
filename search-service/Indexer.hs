module Indexer
  ( indexFeatures
  ) where

import Data.Text (pack)
import qualified Features.SearchableFeature as SF

indexFeatures :: [FilePath] -> IO ()
indexFeatures features = do
  searchableFeatures <- buildSearchableFeatures features
  replies            <- SF.indexFeatures searchableFeatures
  -- this could become useful as log output
  putStrLn $ foldr (\x acc -> acc ++ "\n" ++ (show x)) "" replies

buildSearchableFeatures :: [FilePath] -> IO [SF.SearchableFeature]
buildSearchableFeatures filePaths = do
  -- this could be parallelized
  sequence $ map ((fmap buildSearchableFeature) . getFileContents) filePaths

getFileContents :: FilePath -> IO (FilePath, String)
getFileContents filePath = do
  fileContents <- readFile filePath
  return (filePath, fileContents)

buildSearchableFeature :: (FilePath, String) -> SF.SearchableFeature
buildSearchableFeature (filePath, fileContents) =
  SF.SearchableFeature { SF.featurePath = pack filePath
                       , SF.featureText = pack fileContents
                       }
