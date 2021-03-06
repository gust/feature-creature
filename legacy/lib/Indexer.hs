module Indexer
( indexFeatures
, deleteFeatures
) where

import Config.Config (ElasticSearchConfig, GitConfig)
import Control.Exception (IOException, bracket, handle)
import Control.Monad.Except (runExceptT)
import Data.Text (pack)
import qualified Features.Feature as F
import qualified Features.SearchableFeature as SF
import Products.Product (ProductID)
import Products.ProductRepo (codeRepositoryDir)
import System.Directory (doesFileExist)
import System.IO (IOMode (ReadMode), openFile, hClose, hGetContents)

-- create an abstraction here
-- perhapes ElasticSearchConfig -> IO () = WithElasticSearch
-- maybe even GitConfig -> ElasticSearchConfig -> IO () = WithGitSearch

-- FIX: we're not actually taking advantage of the bulk index
-- we are recurring over the list and 'bulk' indexing a single
-- file each time
indexFeatures :: [F.FeatureFile] -> ProductID -> GitConfig -> ElasticSearchConfig -> IO ()
indexFeatures [] _ _ _ = putStrLn "Finished indexing!"
indexFeatures ((F.FeatureFile f):fs) prodID gitConfig esConfig =
  indexFeature f prodID gitConfig esConfig
    >> indexFeatures fs prodID gitConfig esConfig

-- FIX: we're not actually taking advantage of the bulk index
-- we are recurring over the list and 'bulk' indexing a single
-- file each time
deleteFeatures :: [F.FeatureFile]  -> ElasticSearchConfig -> IO ()
deleteFeatures [] _ = putStrLn "Finished deleting!"
deleteFeatures ((F.FeatureFile f):fs) esConfig =
  deleteFeature f esConfig
    >> deleteFeatures fs esConfig

indexFeature :: FilePath -> ProductID -> GitConfig -> ElasticSearchConfig -> IO ()
indexFeature filePath prodID gitConfig esConfig =
  let featureFileBasePath = codeRepositoryDir prodID gitConfig
      fullFilePath        = featureFileBasePath ++ filePath
  in (doesFileExist fullFilePath) >>= \exists ->
      case exists of
        False -> putStrLn $ "File does not exist: " ++ fullFilePath
        True ->
          handle handleIOException $
            bracket (openFile fullFilePath ReadMode) hClose $ \h ->
              hGetContents h >>= \fileContents ->
                let searchableFeature = SF.SearchableFeature (pack filePath) (pack fileContents) prodID
                in (runExceptT $ SF.indexFeatures [searchableFeature] esConfig) >>= \result ->
                     case result of
                       (Left err) -> putStrLn ("Error indexing feature: " ++ err) >> putStrLn (show searchableFeature)
                       (Right _)  -> putStrLn ("Successfully indexed: " ++ filePath)

deleteFeature :: FilePath -> ElasticSearchConfig -> IO ()
deleteFeature filePath esConfig =
  (runExceptT $ SF.deleteFeatures [(pack filePath)] esConfig) >>= \result ->
    case result of
      (Left err) -> putStrLn ("Error deleting index: " ++ err) >> putStrLn filePath
      (Right _)  -> putStrLn ("Successfully deleted: " ++ filePath)

handleIOException :: IOException -> IO ()
handleIOException ex = putStrLn $ "IOExcpetion: " ++ (show ex)
