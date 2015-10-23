module UserRoles.UserRole(findByProductId, createUserRole, findUserRoles, UserRole(..)) where
  import Database (runDB)
  import Data.Int (Int64)
  import qualified Database.Persist.Postgresql as DB
  import Models

  createUserRole :: UserRole -> IO Int64
  createUserRole userRole = (runDB $ DB.insert userRole) >>= return . DB.fromSqlKey

  findUserRoles :: IO [DB.Entity UserRole]
  findUserRoles = runDB $ DB.selectList ([] :: [DB.Filter UserRole]) []

  findByProductId :: ProductId -> IO [DB.Entity UserRole]
  findByProductId productId = runDB $ DB.selectList [UserRoleProductId DB.==. productId] []
