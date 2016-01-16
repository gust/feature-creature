module Indexer
  ( indexFeatures
  ) where

import Data.Text (pack)
import Control.Exception (IOException, bracket, handle)
import qualified Features.SearchableFeature as SF
import System.IO ( IOMode (ReadMode), openFile, hClose, hGetContents)

indexFeatures :: [FilePath] -> IO ()
indexFeatures [] = putStrLn "Finished indexing!"
indexFeatures (f:fs) =
  indexFeature f >> indexFeatures fs

indexFeature :: FilePath -> IO ()
indexFeature filePath = handle handleIOException $
  bracket (openFile filePath ReadMode) hClose $ \h -> do
    fileContents <- hGetContents h
    let searchableFeature = SF.SearchableFeature (pack filePath) (pack fileContents)
    putStrLn $ "Indexing: " ++ (show searchableFeature)
    SF.indexFeatures [searchableFeature]

handleIOException :: IOException -> IO ()
handleIOException ex = putStrLn $ "IOExcpetion: " ++ (show ex)
