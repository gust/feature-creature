module DomainTerms.DomainTerm(findByProductId, createDomainTerm, findDomainTerms, DomainTerm(..)) where
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
