module Features
( indexFeatures
) where

import           CommonCreatures (WithErr)
import           AppConfig (ElasticSearchConfig, GitConfig)
import           Control.Monad.Except (runExceptT, throwError, liftIO)
import           Features.Feature (findFeatureFiles)
import qualified Indexer
import           Products.CodeRepository (CodeRepository(..), codeRepositoryDir)

indexFeatures :: CodeRepository -> GitConfig -> ElasticSearchConfig -> WithErr ()
indexFeatures (CodeRepository productID) gitConfig esConfig = do
  let featureFileBasePath = codeRepositoryDir productID gitConfig
  featureFiles <- liftIO $ runExceptT $ findFeatureFiles featureFileBasePath
  case featureFiles of
    Left errorStr ->
      throwError errorStr
    Right features -> do
      (liftIO $ Indexer.indexFeatures features productID gitConfig esConfig) >> return ()
