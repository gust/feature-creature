module DomainTerms.DomainTerm
( findByProductId
, createDomainTerm
, updateDomainTerm
, removeDomainTerm
, findDomainTerms
, toDomainTermID
, toDomainTerm
, DomainTerm(..)
) where

import Config (DBConfig, getPool)
import Data.Int (Int64)
import qualified Database.Persist.Postgresql as DB
import Models

-- rewrite this using a WithDBConn monad
createDomainTerm :: DBConfig -> DomainTerm -> IO Int64
createDomainTerm dbConfig domainTerm =
  let query = DB.insert domainTerm
      pool = getPool dbConfig
  in
    DB.runSqlPool query pool >>= return . DB.fromSqlKey

updateDomainTerm :: DBConfig -> DomainTermId -> DomainTerm -> IO DomainTerm
updateDomainTerm dbConfig dtId domainTerm@(DomainTerm _ title description) =
  let pool = getPool dbConfig
      query = DB.update
                dtId
                [ DomainTermTitle DB.=. title
                , DomainTermDescription DB.=. description
                ]
  in
    DB.runSqlPool query pool >> return domainTerm

-- rewrite this using a WithDBConn monad
removeDomainTerm :: DBConfig -> ProductId -> DomainTermId -> IO ()
removeDomainTerm dbConfig productID domainTermID =
  let pool = getPool dbConfig
      query = DB.deleteWhere [ DomainTermProductId DB.==. productID
                             , DomainTermId DB.==. domainTermID
                             ]
  in
    DB.runSqlPool query pool >>= return

-- rewrite this using a WithDBConn monad
findDomainTerms :: DBConfig -> IO [DB.Entity DomainTerm]
findDomainTerms dbConfig =
  let query = DB.selectList ([] :: [DB.Filter DomainTerm]) [ DB.Asc DomainTermTitle ]
      pool = getPool dbConfig
  in
    DB.runSqlPool query pool

-- rewrite this using a WithDBConn monad
findByProductId :: DBConfig -> ProductId -> IO [DB.Entity DomainTerm]
findByProductId dbConfig productId =
  let query = DB.selectList [DomainTermProductId DB.==. productId] [ DB.Asc DomainTermTitle ]
      pool = getPool dbConfig
  in
    DB.runSqlPool query pool

toDomainTermID :: DB.Entity DomainTerm -> Int64
toDomainTermID dbEntity = DB.fromSqlKey . DB.entityKey $ dbEntity

toDomainTerm :: DB.Entity DomainTerm -> DomainTerm
toDomainTerm dbEntity = DB.entityVal dbEntity
