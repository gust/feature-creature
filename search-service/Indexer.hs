module Indexer
  ( indexFeatures
  ) where

import Data.Text (pack)
import Config (GitConfig)
import Control.Exception (IOException, bracket, handle)
import qualified Features.SearchableFeature as SF
import Products.CodeRepository (codeRepositoryDir)
import Products.Product (ProductID)
import System.IO ( IOMode (ReadMode), openFile, hClose, hGetContents)

indexFeatures :: [FilePath] -> ProductID -> GitConfig -> IO ()
indexFeatures [] _ _ = putStrLn "Finished indexing!"
indexFeatures (f:fs) prodID gitConfig =
  indexFeature f prodID gitConfig >> indexFeatures fs prodID gitConfig

indexFeature :: FilePath -> ProductID -> GitConfig -> IO ()
indexFeature filePath prodID gitConfig =
  let featureFileBasePath = codeRepositoryDir prodID gitConfig
      fullFilePath        = featureFileBasePath ++ filePath
  in
    handle handleIOException $
      bracket (openFile fullFilePath ReadMode) hClose $ \h -> do
        fileContents <- hGetContents h
        let searchableFeature = SF.SearchableFeature (pack filePath) (pack fileContents) prodID
        putStrLn $ "Indexing: " ++ (show searchableFeature)
        SF.indexFeatures [searchableFeature]

handleIOException :: IOException -> IO ()
handleIOException ex = putStrLn $ "IOExcpetion: " ++ (show ex)
