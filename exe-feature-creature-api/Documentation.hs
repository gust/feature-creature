{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Documentation
( DocumentationAPI
, documentationServer
) where

import App
import Data.DirectoryTree
import Data.Int (Int64)
import qualified Data.List as L
import Data.Text (Text, pack)
import Data.Tree (Tree(Node))
import qualified Features.Feature as F
import Api.MimeTypes (Markdown)
import Models
import ModelTypes
import Products.DomainTermsAPI as DT (APIDomainTerm (..))
import Products.FeaturesAPI as F (APIFeature (..))
import Products.ProductsAPI as P (productsAPI)
import Products.UserRolesAPI as U (APIUserRole (..))
import Products.ProductRepo as PR (ProductRepo (..))
import Servant
import Servant.Docs

type DocumentationAPI = "docs" :> Get '[Markdown] Text

instance ToCapture (Capture "id" Int64) where
  toCapture _ = DocCapture "id" "A database entity ID"

instance ToSample ProductRepo where
  toSamples _ = singleSample sampleCreatureProductRepo

instance ToSample APIFeature where
  toSamples _ = singleSample $
    APIFeature { featureID = F.FeatureFile "/features/werewolves/hunting.feature"
               , F.description = featureFileSample
               }

instance ToParam (QueryParam "path" F.FeatureFile) where
  toParam _ =
    DocQueryParam
    "path"
    [ "/features/werewolf/transformation.feature",
      "/features/swampthing/regeneration.feature"
    ]
    "FeatureFile id (relative file path)"
    Normal

instance ToSample DirectoryTree where
  toSamples _ = singleSample featureDirectoryExample

instance ToParam (QueryParam "search" String) where
  toParam _ = DocQueryParam "search"
                            ["Wolfman", "Creature+From+The+Black+Lagoon", "Creature+Repelent"]
                            "A search term"
                            Normal

instance ToSample APIDomainTerm where
  toSamples _ = singleSample sampleDomainTermPostBody

instance ToSample APIUserRole where
  toSamples _ = singleSample sampleUserRolePostBody

documentationServer :: App Text
documentationServer = return (pack documentation)

documentation :: String
documentation = markdown (docsWithIntros [intro] productsAPI)

intro :: DocIntro
intro = DocIntro "feature-creature"
  ["![](http://www.homecinemachoice.com/sites/18/images/article_images_month/2012-07/universal%20monsters%20news%2001.jpg)"
  , "Welcome to our API"
  , "Feel free to dig around"
  ]

sampleCreatureProductRepo :: ProductRepo
sampleCreatureProductRepo =
  ProductRepo { PR.getProductId        = Just 2
              , PR.getProductName      = "creatures"
              , PR.getProductRepoUrl   = "ssh://creatures.com/repo.git"
              , PR.getProductRepoState = Ready
              , PR.getProductRepoError = Nothing
              }

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

featureDirectoryExample :: DirectoryTree
featureDirectoryExample = DirectoryTree rootNode
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

sampleDomainTermPostBody :: APIDomainTerm
sampleDomainTermPostBody = APIDomainTerm (Just 2) (Just (toKey (10::Integer))) "monsterism" "The quality of being a monster"

sampleUserRolePostBody :: APIUserRole
sampleUserRolePostBody = APIUserRole { U.userRoleID  = Nothing
                             , U.productID   = Just (toKey (10::Integer))
                             , U.title       = "monster magnet"
                             , U.description = "A person or object which attracts monsters"
                             }
