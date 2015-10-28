module CLI.UserRolesForm(execUserRolesCommand) where
  import qualified CLI.DataFiles as Paths
  import Data.Text(pack)
  import Data.List (intersperse)
  import Models(ProductId, toKey, UserRole, Entity)
  import qualified UserRoles.UserRole as UserRole

  execUserRolesCommand :: [String] -> IO ()
  execUserRolesCommand ("add":_) = showCreateUserRoleForm
  execUserRolesCommand ("list":[]) = listUserRoles Nothing
  execUserRolesCommand ("list":productIdStr:[]) = listUserRoles $ Just productIdStr
  execUserRolesCommand _ = showUserRolesCommandUsage

  showCreateUserRoleForm :: IO ()
  showCreateUserRoleForm = do 
    productId   <- (putStrLn "What is the id of the product this is for? ") >> getLine
    name        <- (putStrLn "What is the name of the role you want to define? ") >> getLine
    description <- (putStrLn "Describe the role: ") >> getLine
    userRoleId          <- UserRole.createUserRole $ UserRole.UserRole (toKey . read $ productId) (pack name) (pack description)
    putStrLn $ "Successfully created User role!" ++ show userRoleId

  showUserRolesCommandUsage :: IO ()
  showUserRolesCommandUsage = Paths.showUserRolesCommandUsageFile

  listUserRoles :: Maybe String ->  IO ()
  listUserRoles maybeProductIdStr = findUserRoles maybeProductIdStr >>= printRoles
    where
      findUserRoles :: Maybe String -> IO [Entity UserRole]
      findUserRoles (Just productIdStr) = UserRole.findByProductId $ toProductId productIdStr
      findUserRoles Nothing = UserRole.findUserRoles

      toProductId :: String -> Models.ProductId
      toProductId = Models.toKey . fromIntegral . read

      printRoles roles = putStrLn $ concat . (intersperse "\n") $ map show roles
