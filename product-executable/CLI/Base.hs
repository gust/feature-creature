module CLI.Base where
  import qualified CLI.ProductForm as PF

  data Command = Command { command :: String, arguments :: [String] } 
    deriving (Show)

  execCommand :: Command -> IO ()
  execCommand (Command "product" args) = PF.execProductCommand args
  execCommand (Command _ _) = showUsage

  parseCommand :: [String] -> Maybe Command
  parseCommand [] = Nothing
  parseCommand (x:xs) = Just (Command x xs)

  showUsage :: IO ()
  showUsage = putStrLn "showUsage"

