module UserRoles.UserRole
( findByProductId
, createUserRole
, updateUserRole
, removeUserRole
, findUserRole
, toUserRoleID
, toUserRole
, UserRole(..)
) where

import Control.Monad.Reader (ask, liftIO)
import Data.Int (Int64)
import qualified Database.Persist.Postgresql as DB
import Database.Types (WithDBPool (..))
import Models

createUserRole :: UserRole -> WithDBPool Int64
createUserRole userRole = ask
  >>= liftIO . (DB.runSqlPool (DB.insert userRole))
  >>= return . DB.fromSqlKey

updateUserRole :: UserRoleId -> UserRole -> WithDBPool UserRole
updateUserRole urId ur@(UserRole _ title description _) = ask
  >>= liftIO . (DB.runSqlPool updateUserRoleCommand)
  >> return ur
  where
    updateUserRoleCommand =
      DB.update urId [ UserRoleTitle DB.=. title
                     , UserRoleDescription DB.=. description
                     ]

removeUserRole :: ProductId -> UserRoleId -> WithDBPool ()
removeUserRole pID urID = ask
  >>= liftIO . (DB.runSqlPool deleteUserRoleCommand)
  >>= return
  where
    deleteUserRoleCommand =
      DB.deleteWhere [ UserRoleProductId DB.==. pID
                     , UserRoleId DB.==. urID
                     ]


findUserRole :: UserRoleId -> WithDBPool (Maybe UserRole)
findUserRole urID = ask
  >>= liftIO . (DB.runSqlPool (DB.get urID))

findByProductId :: ProductId -> WithDBPool [DB.Entity UserRole]
findByProductId productId = ask
  >>= liftIO . (DB.runSqlPool findByProductIdQuery)
  where
    findByProductIdQuery =
      DB.selectList
        [UserRoleProductId DB.==. productId]
        [ DB.Asc UserRoleTitle ]

toUserRoleID :: DB.Entity UserRole -> Int64
toUserRoleID dbEntity = DB.fromSqlKey . DB.entityKey $ dbEntity

toUserRole :: DB.Entity UserRole -> UserRole
toUserRole dbEntity = DB.entityVal dbEntity
