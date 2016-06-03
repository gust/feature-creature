{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Products.FeaturesAPI
( FeaturesAPI
, FeatureAPI
, APIFeature (..)
, productsFeatures
, productsFeature
) where

import Api.Types.Feature
import App
import AppConfig
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as L
import Data.DirectoryTree
import Data.Text (pack, unpack)
import qualified Features.Feature as F
import qualified Network.HTTP.Client.Internal as HTTP
import qualified Network.Wreq as Wreq
import qualified Products.Product as P
import Servant

type FeaturesAPI = "features" :> QueryParam "search" String :> Get '[JSON] DirectoryTree
type FeatureAPI  = "feature"  :> QueryParam "path" F.FeatureFile :> Get '[JSON] APIFeature

productsFeatures :: P.ProductID -> Maybe String -> App DirectoryTree
productsFeatures prodID (Just searchTerm) =
  searchFeatures prodID searchTerm >>= parseProductsResponse
productsFeatures prodID Nothing =
  getFeatures prodID >>= parseProductsResponse

productsFeature :: P.ProductID -> Maybe F.FeatureFile -> App APIFeature
productsFeature _ Nothing = error "Missing required query param 'path'"
productsFeature prodID (Just featureFile) =
  getFeature prodID featureFile >>= parseProductsResponse

getFeature :: P.ProductID -> F.FeatureFile -> App (HTTP.Response L.ByteString)
getFeature prodID featureFile =
  reader featuresAPI >>= \featuresUrl ->
    liftIO $ Wreq.getWith (pathParams featureFile) (getFeatureUrl (unpack featuresUrl) prodID)

getFeatures :: P.ProductID -> App (HTTP.Response L.ByteString)
getFeatures prodID =
  reader featuresAPI >>= \featuresUrl ->
    liftIO $ Wreq.get (getFeaturesUrl (unpack featuresUrl) prodID)

searchFeatures :: P.ProductID -> String -> App (HTTP.Response L.ByteString)
searchFeatures prodID searchTerm =
  reader featuresAPI >>= \featuresUrl ->
    liftIO $ Wreq.getWith (searchParams searchTerm) (getFeaturesUrl (unpack featuresUrl) prodID)

parseProductsResponse :: Aeson.FromJSON a => HTTP.Response L.ByteString -> App a
parseProductsResponse resp =
  case Aeson.eitherDecode (resp ^. Wreq.responseBody) of
    (Left err) -> lift $ throwError $ err503 { errReasonPhrase = err }
    (Right tree) -> return tree

searchParams :: String -> Wreq.Options
searchParams searchTerm = Wreq.defaults & Wreq.param "search" .~ [pack searchTerm]

pathParams :: F.FeatureFile -> Wreq.Options
pathParams (F.FeatureFile path) = Wreq.defaults & Wreq.param "path" .~ [pack path]

getFeaturesUrl :: String -> P.ProductID -> String
getFeaturesUrl featuresUrl prodID = featuresUrl ++ "/products/" ++ (show prodID) ++ "/features"

getFeatureUrl :: String -> P.ProductID -> String
getFeatureUrl featureUrl prodID = featureUrl ++ "/products/" ++ (show prodID) ++ "/feature"

