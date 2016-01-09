module Indexer
  ( indexFeatures
  ) where

import Data.Text (pack)
import qualified Features.SearchableFeature as SF

-- ignores failure
indexFeatures :: [FilePath] -> IO ()
indexFeatures features =
  buildSearchableFeatures features >>= SF.indexFeatures

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
