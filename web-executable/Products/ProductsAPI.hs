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
import AppConfig (getDBConfig, getGitConfig, getRabbitMQConfig)
import Async.Job (Job)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader
import Control.Monad.Trans.Either (left)
import Data.Aeson
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as BS
import Models
import Network.AMQP.MessageBus              as MB
import qualified Products.CodeRepository    as CR
import qualified Products.DomainTermsAPI    as DT
import qualified Products.FeaturesAPI       as F
import qualified Products.Product           as P
import qualified Products.Messaging         as PM
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
createProduct (APIProduct _ prodName prodRepoUrl) = do
  let newProduct = P.Product prodName prodRepoUrl
  prodID <- reader getDBConfig >>= liftIO . (P.createProduct newProduct)
  result <- reader getGitConfig >>= liftIO . runExceptT . (CR.updateRepo newProduct prodID)
  case result of
    Left err ->
      lift $ left $ err503 { errBody = BS.pack err }
    Right _ ->
      let job        = CR.indexProductFeaturesJob $ CR.CodeRepository prodID
          apiProduct = APIProduct { productID = Just prodID, name = prodName, repoUrl = prodRepoUrl }
      in (reader getRabbitMQConfig) >>= \rabbitCfg ->
           (liftIO $ MB.withConn rabbitCfg (enqueueMessage job)) >> (return apiProduct)

enqueueMessage :: Job CR.CodeRepository -> WithConn ()
enqueueMessage job =
  PM.subscribeToProductCreation
    >> MB.produceTopicMessage (PM.productCreatedTopic PM.API) (MB.Message job)

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
