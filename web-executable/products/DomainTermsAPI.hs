{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Products.DomainTermsAPI where
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

  data APIDomainTerm = APIDomainTerm { domainTermID :: Int64
                                     , productID    :: ProductId
                                     , title        :: T.Text
                                     , description  :: T.Text
                                     } deriving (Show)

  type DomainTermsAPI = "domain-terms"
                        :> Get '[JSON] [APIDomainTerm]

  instance ToJSON APIDomainTerm where
    toJSON (APIDomainTerm termID prodID termTitle termDescription) =
      object [ "id"          .= termID
             , "productID"   .= prodID
             , "title"       .= termTitle
             , "description" .= termDescription
             ]

  instance SD.ToSample [APIDomainTerm] [APIDomainTerm] where
    toSample _ = Just $
      [ APIDomainTerm 1 (toKey 10) "mutation" "The genetic alteration granting monster powers"
      , APIDomainTerm 2 (toKey 10) "vampirism" "The disease affecting Vampires"
      ]

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

