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
import Products.DomainTermsAPI as DT (APIDomainTerm (..))
import Products.FeaturesAPI as F (APIFeature (..))
import Products.ProductsAPI as P (APIProduct (..), productsAPI)
import Products.UserRolesAPI as U (APIUserRole (..))
import Servant
import qualified Servant.Docs as SD

type DocumentationAPI = "docs" :> Get '[Markdown] Text

instance SD.ToSample () () where
  toSample _ = Just ()

instance SD.ToCapture (Capture "id" Int64) where
  toCapture _ = SD.DocCapture "id" "A database entity ID"

instance SD.ToSample [APIProduct] [APIProduct] where
  toSample _ = Just $ [ sampleMonsterProduct, sampleCreatureProduct ]

instance SD.ToSample APIProduct APIProduct where
  toSample _ = Just sampleCreatureProduct

instance SD.ToSample APIFeature APIFeature where
  toSample _ =
    Just $ APIFeature { featureID = F.FeatureFile "/features/werewolves/hunting.feature"
                      , F.description = featureFileSample
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

instance SD.ToSample [APIDomainTerm] [APIDomainTerm] where
  toSample _ = Just $ [ sampleAPIDomainTerm, sampleAPIDomainTerm2 ]

instance SD.ToSample APIDomainTerm APIDomainTerm where
  toSample _ = Just $ sampleDomainTermPostBody

instance SD.ToSample [APIUserRole] [APIUserRole] where
  toSample _ = Just $ [ sampleMonsterMaker, sampleMonsterHunter ]

instance SD.ToSample APIUserRole APIUserRole where
  toSample _ = Just $ sampleUserRolePostBody

documentationServer :: App Text
documentationServer = return (pack documentation)

documentation :: String
documentation = SD.markdown (SD.docsWithIntros [intro] productsAPI)

intro :: SD.DocIntro
intro = SD.DocIntro "feature-creature" ["![](http://www.homecinemachoice.com/sites/18/images/article_images_month/2012-07/universal%20monsters%20news%2001.jpg)", "Welcome to our API", "Feel free to dig around"]

sampleMonsterProduct :: APIProduct
sampleMonsterProduct = APIProduct { P.productID = Just 1
                                  , name      = "monsters"
                                  , repoUrl   = "http://monsters.com/repo.git"
                                  }

sampleCreatureProduct :: APIProduct
sampleCreatureProduct = APIProduct { P.productID = Just 2
                                   , name      = "creatures"
                                   , repoUrl   = "ssh://creatures.com/repo.git"
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

sampleAPIDomainTerm :: APIDomainTerm
sampleAPIDomainTerm = APIDomainTerm (Just 1) (Just (toKey (10::Integer))) "mutation" "The genetic alteration granting monster powers"

sampleAPIDomainTerm2 :: APIDomainTerm
sampleAPIDomainTerm2 = APIDomainTerm (Just 2) (Just (toKey (10::Integer))) "vampirism" "The disease affecting Vampires"

sampleDomainTermPostBody :: APIDomainTerm
sampleDomainTermPostBody = APIDomainTerm (Just 2) (Just (toKey (10::Integer))) "monsterism" "The quality of being a monster"

sampleMonsterMaker :: APIUserRole
sampleMonsterMaker = APIUserRole { U.userRoleID  = Just 1
                                 , U.productID   = Just (toKey (10::Integer))
                                 , U.title       = "monster maker"
                                 , U.description = "A scientist responsible for creating abominable life forms."
                                 }

sampleMonsterHunter :: APIUserRole
sampleMonsterHunter = APIUserRole { U.userRoleID  = Just 2
                                  , U.productID   = Just (toKey (10::Integer))
                                  , U.title       = "monster hunter"
                                  , U.description = "A hunter specializing in the elimination of monsters."
                                  }

sampleUserRolePostBody :: APIUserRole
sampleUserRolePostBody = APIUserRole { U.userRoleID  = Nothing
                             , U.productID   = Just (toKey (10::Integer))
                             , U.title       = "monster magnet"
                             , U.description = "A person or object which attracts monsters"
                             }
