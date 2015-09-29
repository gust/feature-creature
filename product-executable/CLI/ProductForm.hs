module CLI.ProductForm where
  import qualified CLI.DataFiles as Paths

  execProductCommand :: [String] -> IO ()
  execProductCommand (cmd:_) 
    | cmd == "add"      = showCreateProductForm 
    | otherwise         = showProductCommandUsage
  execProductCommand [] = showProductCommandUsage

  showCreateProductForm :: IO ()
  showCreateProductForm = do
    productName <- (putStrLn "Project Name: ") >> getLine
    putStrLn $ "Project " ++ productName ++ " created!"

  showProductCommandUsage :: IO ()
  showProductCommandUsage = Paths.showProductCommandUsageFile

