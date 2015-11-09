{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ProductsAPI
  ( ProductsAPI
  , productsAPI
  , productsServer
  ) where

  import Control.Monad.Except (runExceptT)
  import Control.Monad.IO.Class (liftIO)
  import Control.Monad.Trans.Either
  import Data.Aeson
  import Data.DirectoryTree
  import qualified Data.List        as L
  import qualified Data.Text        as T
  import Data.Tree (Tree(Node))
  import qualified Features.Feature as F
  import qualified Products.Product as P
  import Servant
  import qualified Servant.Docs     as SD
  import Models

  data APIProduct = APIProduct { productID :: P.ProductID
                               , name      :: T.Text
                               , repoUrl   :: T.Text
                               } deriving (Show)

  data APIFeature = APIFeature { featureID :: F.FeatureFile
                               , description :: F.Feature
                               }

  type Handler a           = EitherT ServantErr IO a

  type ProductsAPI = "products" :> ( Get '[JSON] [APIProduct]
                                :<|> Capture "id" P.ProductID :> FeaturesAPI
                                :<|> Capture "id" P.ProductID :> FeatureAPI
                                   )

  type FeaturesAPI = "features" :> Get '[JSON] DirectoryTree

  type FeatureAPI  = "feature"  :> QueryParam "path" F.FeatureFile
                                :> Get '[JSON] APIFeature


  instance ToJSON APIProduct where
    toJSON (APIProduct prodID prodName prodRepoUrl) =
      object [ "id" .= prodID
             , "name"    .= prodName
             , "repoUrl" .= prodRepoUrl
             ]

  instance ToJSON APIFeature where
    toJSON (APIFeature featID desc) =
      object [ "featureID"   .= featID
             , "description" .= desc
             ]

  instance SD.ToSample [APIProduct] [APIProduct] where
    toSample _ = Just $
      [ APIProduct 1 "monsters" "http://monsters.com/repo.git"
      , APIProduct 2 "creatures" "ssh://creatures.com/repo.git"
      ]

  instance SD.ToSample DirectoryTree DirectoryTree where
    toSample _ = Just featureDirectoryExample

  instance SD.ToSample APIFeature APIFeature where
    toSample _ = Just $ APIFeature { featureID = "/features/werewolves/hunting.feature"
                                   , description = featureFileSample
                                   }

  instance SD.ToCapture (Capture "id" P.ProductID) where
    toCapture _ = SD.DocCapture "id" "Product id"

  instance SD.ToParam (QueryParam "path" F.FeatureFile) where
    toParam _ = SD.DocQueryParam
      "path"
      [ "/features/werewolf/transformation.feature",
        "/features/swampthing/regeneration.feature"
      ]
      "FeatureFile id (relative file path)"
      SD.Normal

  productsServer :: Server ProductsAPI
  productsServer = products
              :<|> productsFeatures
              :<|> productsFeature

  productsAPI :: Proxy ProductsAPI
  productsAPI = Proxy

  products :: Handler [APIProduct]
  products = do
    prods <- liftIO P.findProducts
    return $ map toProduct prods
      where
        toProduct dbProduct = do
          let dbProd   = P.toProduct dbProduct
          let dbProdID = P.toProductID dbProduct
          APIProduct { productID = dbProdID
                     , name      = productName dbProd
                     , repoUrl   = productRepoUrl dbProd }

  productsFeatures :: P.ProductID -> Handler DirectoryTree
  productsFeatures prodID = do
    prodDir <- liftIO $ P.productRepositoryDir prodID
    result  <- liftIO $ runExceptT (F.getFeatures prodDir)
    case result of
      Left msg -> error msg
      Right tree -> return tree

  productsFeature :: P.ProductID -> Maybe F.FeatureFile -> Handler APIFeature
  productsFeature _ Nothing = do
    error "Missing required query param 'path'"
  productsFeature prodID (Just path) = do
    prodDir <- liftIO $ P.productRepositoryDir prodID
    result  <- liftIO $ runExceptT (F.getFeature (prodDir ++ path))
    case result of
      Left msg -> error msg
      Right feature -> return $ APIFeature { featureID = path
                                           , description = feature
                                           }

  featureDirectoryExample :: DirectoryTree
  featureDirectoryExample = rootNode
    where
      rootNode = Node (FileDescription "features" "features") [creatures]
      creatures = Node (FileDescription "creatures" "features/creatures") [swampThing, wolfman]
      swampThing = Node (FileDescription "swamp-thing" "features/creatures/swamp-thing") [
        Node (FileDescription "vegetable-mind-control.feature" "features/creatures/swamp-thing/vegetable-mind-control.feature") [],
        Node (FileDescription "limb-regeneration.feature" "features/creatures/swamp-thing/limb-regeneration.feature") []
        ]
      wolfman = Node (FileDescription "wolfman" "features/creatures/wolfman") [
        Node (FileDescription "shape-shifting.feature" "features/creatures/wolfman/shape-shifting.feature") [],
        Node (FileDescription "animal-instincts.feature" "features/creatures/wolfman/animal-instincts.feature") []
        ]

  featureFileSample :: F.Feature
  featureFileSample = concat . (L.intersperse "\n") $ [ "@some-feature-tag"
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
