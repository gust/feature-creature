{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Features.FeaturesAPI
( FeaturesAPI
, APIFeature (..)
, featuresAPI
, featuresServer
) where

import Api.Types.Feature
import App
import AppConfig (ElasticSearchConfig, getGitConfig, getElasticSearchConfig)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader
import Data.DirectoryTree
import Data.Text (Text, pack, unpack)
import qualified Features.Feature as F
import qualified Features.SearchableFeature as SF
import qualified Products.Product as P
import qualified Products.ProductRepo as PR
import Servant

type FeaturesAPI = "products" :> ProductIDCapture :> "features" :> QueryParam "search" String :> Get '[JSON] DirectoryTree
              :<|> "products" :> ProductIDCapture :> "feature"  :> QueryParam "path" F.FeatureFile :> Get '[JSON] APIFeature

type ProductIDCapture = Capture "id" P.ProductID

featuresServer :: ServerT FeaturesAPI App
featuresServer = getProductFeatures :<|> getProductFeature

featuresAPI :: Proxy FeaturesAPI
featuresAPI = Proxy

getProductFeatures :: P.ProductID -> Maybe String -> App DirectoryTree
getProductFeatures prodID (Just searchTerm) = do
  esConfig <- reader getElasticSearchConfig
  liftIO $ searchFeatures prodID (pack searchTerm) esConfig
getProductFeatures prodID Nothing = do
  featuresPath <- PR.codeRepositoryDir prodID <$> reader getGitConfig
  result       <- liftIO $ runExceptT (F.getFeatures featuresPath)
  case result of
    Left msg   -> error msg
    Right tree -> return tree

searchFeatures :: P.ProductID -> Text -> ElasticSearchConfig -> IO DirectoryTree
searchFeatures prodID searchTerm esConfig =
  (liftIO $ runExceptT (SF.searchFeatures prodID searchTerm esConfig)) >>= \fs ->
    case fs of
      (Left err) -> error err
      (Right features) -> return $ F.buildDirectoryTree . parseFeatureFiles $ features

parseFeatureFiles :: [SF.SearchableFeature] -> [F.FeatureFile]
parseFeatureFiles = map (F.FeatureFile . unpack . SF.getFeaturePath)

getProductFeature :: P.ProductID -> Maybe F.FeatureFile -> App APIFeature
getProductFeature _ Nothing = error "Missing required query param 'path'"
getProductFeature prodID (Just (F.FeatureFile path)) = do
  featuresPath <- PR.codeRepositoryDir prodID <$> reader getGitConfig
  result       <- liftIO $ runExceptT (F.getFeature $ F.FeatureFile (featuresPath ++ path))
  case result of
    Left msg      -> lift $ throwError $ err503 { errReasonPhrase = msg }
    Right feature ->
      return $ APIFeature { featureID = F.FeatureFile path
                          , description = feature
                          }

