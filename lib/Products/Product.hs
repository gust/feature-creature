module Products.Product
( Product(..)
, ProductID
, createProduct
, findProducts
, toProduct
, toProductID
) where

import Config (DBConfig, getPool)
import qualified Database.Persist.Postgresql as DB
import GHC.Int (Int64)
import Models

type ProductID = Int64

-- rewrite this using a WithDBConn monad
createProduct :: Product -> DBConfig -> IO ProductID
createProduct p dbConfig =
  let query = DB.insert p
      pool = getPool dbConfig
  in
    DB.runSqlPool query pool >>= return . DB.fromSqlKey

-- rewrite this using a WithDBConn monad
findProducts :: DBConfig -> IO [DB.Entity Product]
findProducts dbConfig =
  let query = DB.selectList ([] :: [DB.Filter Product]) []
      pool = getPool dbConfig
  in
    DB.runSqlPool query pool

toProductID :: DB.Entity Product -> ProductID
toProductID dbEntity =
  DB.fromSqlKey . DB.entityKey $ dbEntity

toProduct :: DB.Entity Product -> Product
toProduct dbEntity =
  DB.entityVal dbEntity
