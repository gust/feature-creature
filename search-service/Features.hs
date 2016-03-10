module Features
( indexFeatures
) where

import CommonCreatures (WithErr)
import AppConfig (ElasticSearchConfig, GitConfig)
import Control.Monad.IO.Class (liftIO)
import Features.Feature (FeatureFile, findFeatureFiles)
import qualified Indexer
import Products.CodeRepository (CodeRepository(..), codeRepositoryDir)
import Products.Product (ProductID)

indexFeatures :: CodeRepository -> GitConfig -> ElasticSearchConfig -> WithErr ()
indexFeatures (CodeRepository productID) gitConfig esConfig =
  (findFeatureFiles (codeRepositoryDir productID gitConfig))
    >>= (\featureFiles -> indexFeatures' featureFiles productID gitConfig esConfig)

indexFeatures' :: [FeatureFile] -> ProductID -> GitConfig -> ElasticSearchConfig -> WithErr ()
indexFeatures' features productID gitConfig esConfig =
  (liftIO $ Indexer.indexFeatures features productID gitConfig esConfig)
    >> return ()

