{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Products.DomainTermsAPI
( DomainTermsAPI
, CreateDomainTermsAPI
, EditDomainTermsAPI
, RemoveDomainTermAPI
, APIDomainTerm (..)
, createDomainTerm
, editDomainTerm
, removeDomainTerm
, productsDomainTerms
) where

import App
import AppConfig (DBConfig (..), getDBConfig)
import Control.Monad.Reader
import Control.Monad.Trans.Either (left)
import Data.Aeson
import Data.Int (Int64)
import qualified Data.Text              as T
import Data.Time.Clock as Clock
import Database.Types (runPool)
import qualified DomainTerms.DomainTerm as DT
import Models
import qualified Products.Product as P
import Servant

type DomainTermsAPI       = "domain-terms" :> Get '[JSON] [APIDomainTerm]
type CreateDomainTermsAPI = "domain-terms" :> ReqBody '[JSON] APIDomainTerm :> Post '[JSON] APIDomainTerm
type EditDomainTermsAPI   = "domain-terms" :> Capture "id" Int64 :> ReqBody '[JSON] APIDomainTerm :> Put '[JSON] APIDomainTerm
type RemoveDomainTermAPI  = "domain-terms" :> Capture "id" Int64 :> Delete '[JSON] ()

data APIDomainTerm = APIDomainTerm { domainTermID :: Maybe Int64
                                   , productID    :: Maybe ProductId
                                   , title        :: T.Text
                                   , description  :: T.Text
                                   } deriving (Show)

instance ToJSON APIDomainTerm where
  toJSON (APIDomainTerm termID prodID termTitle termDescription) =
    object [ "id"          .= termID
           , "productID"   .= prodID
           , "title"       .= termTitle
           , "description" .= termDescription
           ]

instance FromJSON APIDomainTerm where
  parseJSON (Object v) = APIDomainTerm <$>
                        v .:? "id" <*>
                        v .:? "productID" <*>
                        v .: "title" <*>
                        v .: "description"
  parseJSON _          = mzero

createDomainTerm :: P.ProductID -> APIDomainTerm -> App APIDomainTerm
createDomainTerm pID (APIDomainTerm _ _ t d) = do
  dbConfig <- reader getDBConfig
  utcTime  <- liftIO $ Clock.getCurrentTime
  termID   <- liftIO $ runReaderT (runPool (DT.createDomainTerm (DT.DomainTerm (toKey pID) t d utcTime))) (getPool dbConfig)
  return $ APIDomainTerm { domainTermID = Just termID
                         , productID    = Just (toKey pID)
                         , title        = t
                         , description  = d
                         }

editDomainTerm :: P.ProductID -> Int64 -> APIDomainTerm -> App APIDomainTerm
editDomainTerm pID dtID (APIDomainTerm _ _ t d) = do
  dbConfig          <- reader getDBConfig
  domainTerm        <- liftIO $ runReaderT (runPool (DT.findDomainTerm (toKey dtID))) (getPool dbConfig)
  case domainTerm of
    Nothing   -> lift $ left $ err404
    (Just dt) ->
      (liftIO $ runReaderT (runPool (DT.updateDomainTerm (toKey dtID) dt)) (getPool dbConfig))
        >> (return $ APIDomainTerm { domainTermID = Just (dtID)
                                   , productID    = Just (toKey pID)
                                   , title        = t
                                   , description  = d
                                   })

removeDomainTerm :: P.ProductID -> Int64 -> App ()
removeDomainTerm pID dtID = do
  dbConfig <- reader getDBConfig
  liftIO $ runReaderT (runPool (DT.removeDomainTerm (toKey pID) (toKey dtID))) (getPool dbConfig)

productsDomainTerms :: P.ProductID -> App [APIDomainTerm]
productsDomainTerms prodID = do
  dbConfig    <- reader getDBConfig
  domainTerms <- liftIO $ runReaderT (runPool (DT.findByProductId (toKey prodID))) (getPool dbConfig)
  return $ map toDomainTerm domainTerms
    where
      toDomainTerm dbDomainTerm = do
        let dbTerm   = DT.toDomainTerm dbDomainTerm
        let dbTermID = DT.toDomainTermID dbDomainTerm
        APIDomainTerm { domainTermID = Just dbTermID
                      , productID    = Just $ domainTermProductId dbTerm
                      , title        = domainTermTitle dbTerm
                      , description  = domainTermDescription dbTerm
                      }

