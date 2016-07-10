{-# LANGUAGE OverloadedStrings #-}

module Api.Types.Feature
( APIFeature (..)
) where


import Control.Monad (mzero)
import Data.Aeson
import qualified Features.Feature as F

data APIFeature = APIFeature { featureID :: F.FeatureFile
                             , description :: F.Feature
                             } deriving (Show)

instance ToJSON APIFeature where
  toJSON (APIFeature (F.FeatureFile featID) (F.Feature desc)) =
    object [ "featureID"   .= featID
           , "description" .= desc
           ]

instance FromJSON APIFeature where
  parseJSON (Object v) = APIFeature <$>
                        v .: "featureID" <*>
                        v .: "description"
  parseJSON _          = mzero

