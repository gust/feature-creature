{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Products.ProductsAPI
( ProductsAPI
, productsAPI
, productsServer
) where

import App
import AppConfig (getDBConfig, getRabbitMQConfig)
import Messaging.Job (Job (..), JobType (..))
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Text                  as T
import qualified Messaging.Products         as MP
import Models
import Network.AMQP.MessageBus              as MB
import qualified Products.DomainTermsAPI    as DT
import qualified Products.FeaturesAPI       as F
import qualified Products.Product           as P
import qualified Products.UserRolesAPI      as UR
import Servant
import qualified Servant.Docs               as SD

type ProductsAPI = "products" :> Get '[JSON] [APIProduct]
              :<|> "products" :> ReqBody '[JSON] APIProduct :> Post '[JSON] APIProduct
              :<|> "products" :> ProductIDCapture :> F.FeaturesAPI
              :<|> "products" :> ProductIDCapture :> F.FeatureAPI
              :<|> "products" :> ProductIDCapture :> DT.DomainTermsAPI
              :<|> "products" :> ProductIDCapture :> DT.CreateDomainTermsAPI
              :<|> "products" :> ProductIDCapture :> DT.EditDomainTermsAPI
              :<|> "products" :> ProductIDCapture :> DT.RemoveDomainTermAPI
              :<|> "products" :> ProductIDCapture :> UR.UserRolesAPI
              :<|> "products" :> ProductIDCapture :> UR.CreateUserRolesAPI
              :<|> "products" :> ProductIDCapture :> UR.EditUserRolesAPI
              :<|> "products" :> ProductIDCapture :> UR.RemoveUserRoleAPI

type ProductIDCapture = Capture "id" P.ProductID

data APIProduct = APIProduct { productID :: Maybe P.ProductID
                             , name      :: T.Text
                             , repoUrl   :: T.Text
                             } deriving (Show)

instance ToJSON APIProduct where
  toJSON (APIProduct prodID prodName prodRepoUrl) =
    object [ "id"      .= prodID
           , "name"    .= prodName
           , "repoUrl" .= prodRepoUrl
           ]

instance FromJSON APIProduct where
  parseJSON (Object v) = APIProduct <$>
                        v .:? "id" <*>
                        v .: "name" <*>
                        v .: "repoUrl"
  parseJSON _          = mzero

productsServer :: ServerT ProductsAPI App
productsServer = products
            :<|> createProduct
            :<|> F.productsFeatures
            :<|> F.productsFeature
            :<|> DT.productsDomainTerms
            :<|> DT.createDomainTerm
            :<|> DT.editDomainTerm
            :<|> DT.removeDomainTerm
            :<|> UR.productsUserRoles
            :<|> UR.createUserRole
            :<|> UR.editUserRole
            :<|> UR.removeUserRole

productsAPI :: Proxy ProductsAPI
productsAPI = Proxy

createProduct :: APIProduct -> App APIProduct
createProduct (APIProduct _ prodName prodRepoUrl) =
  let newProduct = P.Product prodName prodRepoUrl in ask
    >>= \cfg    -> liftIO $ (P.createProduct newProduct (getDBConfig cfg))
    >>= \prodID ->
          let apiProduct = APIProduct (Just prodID) prodName prodRepoUrl
              job        = Job RepositoryCreated apiProduct
          in (liftIO $ MB.withConn (getRabbitMQConfig cfg) (enqueueMessage job))
              >> return apiProduct

enqueueMessage :: ToJSON a => Job a -> WithConn ()
enqueueMessage job =
  MP.subscribeToProductCreation -- we may not need to do this here
    >> MB.produceTopicMessage (MP.productCreatedTopic MP.API) (MB.Message job)

products :: App [APIProduct]
products = do
  prods <- reader getDBConfig >>= liftIO . P.findProducts
  return $ map toProduct prods
    where
      toProduct dbProduct = do
        let dbProd   = P.toProduct dbProduct
        let dbProdID = P.toProductID dbProduct
        APIProduct { productID = Just dbProdID
                   , name      = productName dbProd
                   , repoUrl   = productRepoUrl dbProd }

-- API Documentation Instance Definitions --

instance SD.ToSample [APIProduct] [APIProduct] where
  toSample _ = Just $ [ sampleMonsterProduct, sampleCreatureProduct ]

instance SD.ToSample APIProduct APIProduct where
  toSample _ = Just sampleCreatureProduct

sampleMonsterProduct :: APIProduct
sampleMonsterProduct = APIProduct { productID = Just 1
                                  , name      = "monsters"
                                  , repoUrl   = "http://monsters.com/repo.git"
                                  }

sampleCreatureProduct :: APIProduct
sampleCreatureProduct = APIProduct { productID = Just 2
                                   , name      = "creatures"
                                   , repoUrl   = "ssh://creatures.com/repo.git"
                                   }
