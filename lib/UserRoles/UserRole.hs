module UserRoles.UserRole
( findByProductId
, createUserRole
, findUserRoles
, toUserRoleID
, toUserRole
, UserRole(..)
) where

import Config (DBConfig, getPool)
import Data.Int (Int64)
import qualified Database.Persist.Postgresql as DB
import Models

-- rewrite this using a WithDBConn monad
createUserRole :: DBConfig -> UserRole -> IO Int64
createUserRole dbConfig userRole =
  let query = DB.insert userRole
      pool = getPool dbConfig
  in
    DB.runSqlPool query pool >>= return . DB.fromSqlKey

-- rewrite this using a WithDBConn monad
findUserRoles :: DBConfig -> IO [DB.Entity UserRole]
findUserRoles dbConfig =
  let query = DB.selectList ([] :: [DB.Filter UserRole]) []
      pool = getPool dbConfig
  in
    DB.runSqlPool query pool

-- rewrite this using a WithDBConn monad
findByProductId :: DBConfig -> ProductId -> IO [DB.Entity UserRole]
findByProductId dbConfig productId =
  let query = DB.selectList [UserRoleProductId DB.==. productId] []
      pool = getPool dbConfig
  in
    DB.runSqlPool query pool

toUserRoleID :: DB.Entity UserRole -> Int64
toUserRoleID dbEntity = DB.fromSqlKey . DB.entityKey $ dbEntity

toUserRole :: DB.Entity UserRole -> UserRole
toUserRole dbEntity = DB.entityVal dbEntity
