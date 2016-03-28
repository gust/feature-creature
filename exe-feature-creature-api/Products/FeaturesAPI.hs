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
import Control.Monad.Trans.Either (left)
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Data.DirectoryTree
import Data.Text (pack, unpack)
import qualified Features.Feature as F
import qualified Network.Wreq as Wreq
import qualified Products.Product as P
import Servant

type FeaturesAPI = "features" :> QueryParam "search" String :> Get '[JSON] DirectoryTree
type FeatureAPI  = "feature"  :> QueryParam "path" F.FeatureFile
                              :> Get '[JSON] APIFeature

productsFeatures :: P.ProductID -> Maybe String -> App DirectoryTree
productsFeatures prodID (Just searchTerm) = do
  featuresUrl <- reader featuresAPI
  resp <- liftIO $ Wreq.getWith (Wreq.defaults & Wreq.param "search" .~ [pack searchTerm]) ((unpack featuresUrl) ++ "/products/" ++ (show prodID) ++ "/features")
  case Aeson.eitherDecode (resp ^. Wreq.responseBody) of
    (Left err) -> lift $ left $ err503 { errReasonPhrase = err }
    (Right tree) -> return tree
productsFeatures prodID Nothing = do
  featuresUrl <- reader featuresAPI
  resp <- liftIO $ Wreq.get ((unpack featuresUrl) ++ "/products/" ++ (show prodID) ++ "/features")
  case Aeson.eitherDecode (resp ^. Wreq.responseBody) of
    (Left err) -> lift $ left $ err503 { errReasonPhrase = err }
    (Right tree) -> return tree

productsFeature :: P.ProductID -> Maybe F.FeatureFile -> App APIFeature
productsFeature _ Nothing = error "Missing required query param 'path'"
productsFeature prodID (Just (F.FeatureFile path)) = do
  featuresUrl <- reader featuresAPI
  resp <- liftIO $ Wreq.getWith (Wreq.defaults & Wreq.param "path" .~ [pack path]) ((unpack featuresUrl) ++ "/products/" ++ (show prodID) ++ "/feature")
  case Aeson.eitherDecode (resp ^. Wreq.responseBody) of
    (Left err) -> lift $ left $ err503 { errReasonPhrase = err }
    (Right feature) -> return feature

