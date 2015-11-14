{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Products.DomainTermsAPI where
  import Control.Monad (mzero)
  import Control.Monad.IO.Class (liftIO)
  import Data.Aeson
  import Data.Int (Int64)
  import qualified Data.Text              as T
  import qualified DomainTerms.DomainTerm as DT
  import Models
  import qualified Products.Product as P
  import Servant
  import qualified Servant.Docs     as SD
  import ServantUtilities (Handler)

  type DomainTermsAPI       = "domain-terms" :> Get '[JSON] [APIDomainTerm]
  type CreateDomainTermsAPI = "domain-terms" :> ReqBody '[JSON] APIDomainTerm :> Post '[JSON] APIDomainTerm

  data APIDomainTerm = APIDomainTerm { domainTermID :: Int64
                                     , productID    :: ProductId
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
                          v .: "id" <*>
                          v .: "productID" <*>
                          v .: "title" <*>
                          v .: "description"
    parseJSON _          = mzero

  createDomainTerm :: P.ProductID -> APIDomainTerm -> Handler APIDomainTerm
  createDomainTerm _ apiDomainTerm@(APIDomainTerm _ pID t d) = do
    _ <- liftIO $ DT.createDomainTerm (DT.DomainTerm pID t d)
    return $ apiDomainTerm

  productsDomainTerms :: P.ProductID -> Handler [APIDomainTerm]
  productsDomainTerms prodID = do
    domainTerms <- liftIO $ DT.findByProductId $ toKey prodID
    return $ map toDomainTerm domainTerms
      where
        toDomainTerm dbDomainTerm = do
          let dbTerm   = DT.toDomainTerm dbDomainTerm
          let dbTermID = DT.toDomainTermID dbDomainTerm
          APIDomainTerm { domainTermID = dbTermID
                        , productID    = domainTermProductId dbTerm
                        , title        = domainTermTitle dbTerm
                        , description  = domainTermDescription dbTerm
                        }

  -- API Documentation Instance Definitions --

  instance SD.ToSample [APIDomainTerm] [APIDomainTerm] where
    toSample _ = Just $
      [ APIDomainTerm 1 (toKey (10::Integer)) "mutation" "The genetic alteration granting monster powers"
      , APIDomainTerm 2 (toKey (10::Integer)) "vampirism" "The disease affecting Vampires"
      ]

  instance SD.ToSample APIDomainTerm APIDomainTerm where
    toSample _ = Just $ sampleAPIDomainTerm

  sampleAPIDomainTerm :: APIDomainTerm
  sampleAPIDomainTerm = APIDomainTerm 1 (toKey (10::Integer)) "mutation" "The genetic alteration granting monster powers"
