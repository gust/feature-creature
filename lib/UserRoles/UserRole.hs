module UserRoles.UserRole
( findByProductId
, createUserRole
, updateUserRole
, removeUserRole
, findUserRoles
, findUserRole
, toUserRoleID
, toUserRole
, UserRole(..)
) where

import Config.Config (DBConfig, getPool)
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

updateUserRole :: DBConfig -> UserRoleId -> UserRole -> IO UserRole
updateUserRole dbConfig urId userRole@(UserRole _ title description _) =
  let pool = getPool dbConfig
      query = DB.update
                urId
                [ UserRoleTitle DB.=. title
                , UserRoleDescription DB.=. description
                ]
  in
    DB.runSqlPool query pool >> return userRole

-- rewrite this using a WithDBConn monad
removeUserRole :: DBConfig -> ProductId -> UserRoleId -> IO ()
removeUserRole dbConfig productID userRoleID =
  let pool = getPool dbConfig
      query = DB.deleteWhere [ UserRoleProductId DB.==. productID
                             , UserRoleId DB.==. userRoleID
                             ]
  in
    DB.runSqlPool query pool >>= return

-- rewrite this using a WithDBConn monad
findUserRoles :: DBConfig -> IO [DB.Entity UserRole]
findUserRoles dbConfig =
  let query = DB.selectList ([] :: [DB.Filter UserRole]) [ DB.Asc UserRoleTitle ]
      pool = getPool dbConfig
  in
    DB.runSqlPool query pool

findUserRole :: DBConfig -> UserRoleId -> IO (Maybe UserRole)
findUserRole dbConfig urID =
  let pool = getPool dbConfig
      query = DB.get urID
  in
    DB.runSqlPool query pool

-- rewrite this using a WithDBConn monad
findByProductId :: DBConfig -> ProductId -> IO [DB.Entity UserRole]
findByProductId dbConfig productId =
  let query = DB.selectList [UserRoleProductId DB.==. productId] [ DB.Asc UserRoleTitle ]
      pool = getPool dbConfig
  in
    DB.runSqlPool query pool

toUserRoleID :: DB.Entity UserRole -> Int64
toUserRoleID dbEntity = DB.fromSqlKey . DB.entityKey $ dbEntity

toUserRole :: DB.Entity UserRole -> UserRole
toUserRole dbEntity = DB.entityVal dbEntity
