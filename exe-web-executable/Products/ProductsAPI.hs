{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Products.ProductsAPI
( ProductsAPI
, APIProduct (..)
, productsAPI
, productsServer
) where

import App
import AppConfig (DBConfig (..), getDBConfig, getRabbitMQConfig)
import Messaging.Job as Job (Job (..), JobType (..))
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Text                  as T
import Data.Time.Clock as Clock
import qualified Database.Persist.Postgresql as DB
import Database.Types (runPool)
import qualified Messaging.Products         as MP
import Models
import Network.AMQP.MessageBus              as MB
import qualified Products.DomainTermsAPI    as DT
import qualified Products.FeaturesAPI       as F
import qualified Products.Product           as P
import qualified Products.UserRolesAPI      as UR
import Servant

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
productsServer = getProducts
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
createProduct (APIProduct _ prodName prodRepoUrl) = ask
  >>= \cfg     -> (liftIO Clock.getCurrentTime)
  >>= \utcTime -> saveProduct (P.Product prodName prodRepoUrl utcTime)
  >>= \prodID  ->
        let apiProduct = (APIProduct (Just prodID) prodName prodRepoUrl)
            job = Job Job.ProductCreated apiProduct
        in (liftIO $ MB.withConn (getRabbitMQConfig cfg) (sendProductCreatedMessage job))
            >> return apiProduct

saveProduct :: P.Product -> App P.ProductID
saveProduct p = (reader getDBConfig) >>= \cfg ->
  liftIO $ runReaderT (runPool (P.createProduct p)) (getPool cfg)

getProducts :: App [APIProduct]
getProducts = fetchProducts >>= return . (map toProduct)

toProduct :: DB.Entity Product -> APIProduct
toProduct dbProduct =
  let dbProd   = P.toProduct dbProduct
      dbProdID = P.toProductID dbProduct
  in APIProduct { productID = Just dbProdID
                , name      = productName dbProd
                , repoUrl   = productRepoUrl dbProd
                }

fetchProducts :: App [DB.Entity Product]
fetchProducts = (reader getDBConfig) >>= \cfg ->
  liftIO $ runReaderT (runPool P.findProducts) (getPool cfg)

sendProductCreatedMessage :: ToJSON a => Job a -> WithConn ()
sendProductCreatedMessage job =
  MP.subscribeToProductCreation -- we may not need to do this here
    >> MB.produceTopicMessage (MP.productCreatedTopic MP.API) (MB.Message job)
