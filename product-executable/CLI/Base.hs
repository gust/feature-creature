module CLI.Base 
  ( showUsage
  , parseCommand
  , execCommand
  ) where

  import qualified CLI.DataFiles as Paths
  import qualified CLI.ProductForm as PF

  data Command = Command { command :: String
                         , arguments :: [String] 
                         } deriving (Show)

  execCommand :: Command -> IO ()
  execCommand (Command "product" args) = PF.execProductCommand args
  execCommand (Command "list" _)       = listAllDomains
  execCommand (Command _ _)            = showUsage

  parseCommand :: [String] -> Maybe Command
  parseCommand []     = Nothing
  parseCommand (x:xs) = Just (Command x xs)

  showUsage :: IO ()
  showUsage = Paths.showBaseUsageFile

  listAllDomains :: IO ()
  listAllDomains = Paths.showCommandListFile
