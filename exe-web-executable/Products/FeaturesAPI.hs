{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Products.FeaturesAPI
( FeaturesAPI
, FeatureAPI
, productsFeatures
, productsFeature
) where

import App
import AppConfig (ElasticSearchConfig, getGitConfig, getElasticSearchConfig)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader
import Data.Aeson
import Data.DirectoryTree
import qualified Data.List        as L
import Data.Text (Text, pack, unpack)
import Data.Tree (Tree(Node))
import qualified Features.Feature as F
import qualified Features.SearchableFeature as SF
import qualified Products.CodeRepository as CR
import qualified Products.Product as P
import Servant
import qualified Servant.Docs     as SD

data APIFeature = APIFeature { featureID :: F.FeatureFile
                             , description :: F.Feature
                             } deriving (Show)

type FeaturesAPI = "features" :> QueryParam "search" String :> Get '[JSON] DirectoryTree
type FeatureAPI  = "feature"  :> QueryParam "path" F.FeatureFile
                              :> Get '[JSON] APIFeature

productsFeatures :: P.ProductID -> Maybe String -> App DirectoryTree
productsFeatures prodID (Just searchTerm) = do
  esConfig <- reader getElasticSearchConfig
  liftIO $ searchFeatures prodID (pack searchTerm) esConfig
productsFeatures prodID Nothing = do
  featuresPath <- CR.codeRepositoryDir prodID <$> reader getGitConfig
  result       <- liftIO $ runExceptT (F.getFeatures featuresPath)
  case result of
    Left msg   -> error msg
    Right tree -> return tree

searchFeatures :: P.ProductID -> Text -> ElasticSearchConfig -> IO DirectoryTree
searchFeatures prodID searchTerm esConfig =
  (liftIO $ runExceptT (SF.searchFeatures prodID searchTerm esConfig))
    >>= \fs -> case fs of
                 (Left err) -> error err
                 (Right features) -> return $ F.buildDirectoryTree . parseFeatureFiles $ features

parseFeatureFiles :: [SF.SearchableFeature] -> [F.FeatureFile]
parseFeatureFiles = map (F.FeatureFile . unpack . SF.getFeaturePath)

productsFeature :: P.ProductID -> Maybe F.FeatureFile -> App APIFeature
productsFeature _ Nothing = error "Missing required query param 'path'"
productsFeature prodID (Just (F.FeatureFile path)) = do
  featuresPath <- CR.codeRepositoryDir prodID <$> reader getGitConfig
  result       <- liftIO $ runExceptT (F.getFeature $ F.FeatureFile (featuresPath ++ path))
  case result of
    Left msg      -> error msg
    Right feature ->
      return $ APIFeature { featureID = F.FeatureFile path
                          , description = feature
                          }

featureDirectoryExample :: DirectoryTree
featureDirectoryExample = rootNode
  where
    rootNode   = Node (FileDescription "features" "features") [creatures]
    creatures  = Node (FileDescription "creatures" "features/creatures") [swampThing, wolfman]
    swampThing = Node (FileDescription "swamp-thing" "features/creatures/swamp-thing") [
      Node (FileDescription "vegetable-mind-control.feature" "features/creatures/swamp-thing/vegetable-mind-control.feature") [],
      Node (FileDescription "limb-regeneration.feature" "features/creatures/swamp-thing/limb-regeneration.feature") []
      ]
    wolfman = Node (FileDescription "wolfman" "features/creatures/wolfman") [
      Node (FileDescription "shape-shifting.feature" "features/creatures/wolfman/shape-shifting.feature") [],
      Node (FileDescription "animal-instincts.feature" "features/creatures/wolfman/animal-instincts.feature") []
      ]


instance FromText F.FeatureFile where
  fromText path = Just $ F.FeatureFile (unpack path)

instance ToJSON APIFeature where
  toJSON (APIFeature (F.FeatureFile featID) (F.Feature desc)) =
    object [ "featureID"   .= featID
           , "description" .= desc
           ]

instance SD.ToSample APIFeature APIFeature where
  toSample _ =
    Just $ APIFeature { featureID = F.FeatureFile "/features/werewolves/hunting.feature"
                      , description = featureFileSample
                      }

instance SD.ToParam (QueryParam "path" F.FeatureFile) where
  toParam _ =
    SD.DocQueryParam
    "path"
    [ "/features/werewolf/transformation.feature",
      "/features/swampthing/regeneration.feature"
    ]
    "FeatureFile id (relative file path)"
    SD.Normal

instance SD.ToSample DirectoryTree DirectoryTree where
  toSample _ = Just featureDirectoryExample

instance SD.ToParam (QueryParam "search" String) where
  toParam _ = SD.DocQueryParam "search"
                            ["Wolfman", "Creature+From+The+Black+Lagoon", "Creature+Repelent"]
                            "A search term"
                            SD.Normal

featureFileSample :: F.Feature
featureFileSample =
  F.Feature $ concat . (L.intersperse "\n")
  $ [ "@some-feature-tag"
    , "Feature: Slaying a Werewolf"
    , "  As a Werewolf Hunter"
    , "  So that I can collect my Reward"
    , "  I want to slay a Werewolf"
    , ""
    , "  Scenario: Slaying a Werewolf in human form"
    , "    Given I am a Werewolf Hunter"
    , "    When I slay a Werewolf in human form"
    , "    Then I collect a Reward from the Townspeople"
    , ""
    , "  Scenario: Slaying a Werewolf in wolf form"
    , "    Given I am a Werewolf Hunter"
    , "    When I attempt to slay a Werewolf in wolf form"
    , "    Then I am brutally ripped limb from limb"
    , "    And no Reward is collected"
    ]