module DomainTerms.DomainTerm
  ( findByProductId
  , createDomainTerm
  , findDomainTerms
  , toDomainTermID
  , toDomainTerm
  , DomainTerm(..)
  ) where

  import Database (runDB)
  import Data.Int (Int64)
  import qualified Database.Persist.Postgresql as DB
  import Models

  createDomainTerm :: DomainTerm -> IO Int64
  createDomainTerm domainTerm = (runDB $ DB.insert domainTerm) >>= return . DB.fromSqlKey

  findDomainTerms :: IO [DB.Entity DomainTerm]
  findDomainTerms = runDB $ DB.selectList ([] :: [DB.Filter DomainTerm]) []

  findByProductId :: ProductId -> IO [DB.Entity DomainTerm]
  findByProductId productId = runDB $ DB.selectList [DomainTermProductId DB.==. productId] []

  toDomainTermID :: DB.Entity DomainTerm -> Int64
  toDomainTermID dbEntity = DB.fromSqlKey . DB.entityKey $ dbEntity

  toDomainTerm :: DB.Entity DomainTerm -> DomainTerm
  toDomainTerm dbEntity = DB.entityVal dbEntity
