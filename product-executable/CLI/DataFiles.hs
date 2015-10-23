module CLI.DataFiles where
  import qualified Paths_feature_creature as Paths

  showBaseUsageFile :: IO ()
  showBaseUsageFile = do
    filePath <- getBaseUsageFilePath
    showFileContents filePath

  getBaseUsageFilePath :: IO String
  getBaseUsageFilePath = Paths.getDataFileName "data/cli/base-usage.txt"



  showCommandListFile :: IO ()
  showCommandListFile = do
    filePath <- getCommandListFilePath
    showFileContents filePath

  getCommandListFilePath :: IO String
  getCommandListFilePath = Paths.getDataFileName "data/cli/command-list.txt"



  showProductCommandUsageFile:: IO ()
  showProductCommandUsageFile =  do
      filePath <- getProductCommandUsageFilePath
      showFileContents filePath

  getProductCommandUsageFilePath :: IO String
  getProductCommandUsageFilePath = Paths.getDataFileName "data/cli/product-command-usage.txt"



  showUserRolesCommandUsageFile:: IO ()
  showUserRolesCommandUsageFile = getUserRolesCommandUsageFilePath >>= showFileContents

  getUserRolesCommandUsageFilePath :: IO String
  getUserRolesCommandUsageFilePath = Paths.getDataFileName "data/cli/user-roles-command-usage.txt"



  showFileContents :: FilePath -> IO ()
  showFileContents filePath = do
    fileContents <- readFile filePath
    putStr fileContents
