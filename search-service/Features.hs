module Features
( indexFeatures
) where

import CommonCreatures (WithErr)
import AppConfig (ElasticSearchConfig, GitConfig)
import Control.Monad.Except (runExceptT, throwError, liftIO)
import Features.Feature (FeatureFile, findFeatureFiles)
import qualified Indexer
import Products.CodeRepository (CodeRepository(..), codeRepositoryDir)
import Products.Product (ProductID)

indexFeatures :: CodeRepository -> GitConfig -> ElasticSearchConfig -> WithErr ()
indexFeatures (CodeRepository productID) gitConfig esConfig = do
  (liftIO $ runExceptT $ findFeatureFiles (codeRepositoryDir productID gitConfig))
    >>= (\featureFiles -> indexFeatures' featureFiles productID gitConfig esConfig)

indexFeatures' :: Either String [FeatureFile] -> ProductID -> GitConfig -> ElasticSearchConfig -> WithErr ()
indexFeatures' (Left err) _ _ _   = throwError err
indexFeatures' (Right features) productID gitConfig esConfig =
  (liftIO $ Indexer.indexFeatures features productID gitConfig esConfig)
    >> return ()

