module CLI.ProductForm where
  import qualified CLI.DataFiles as Paths
  import qualified Products.Product as P

  execProductCommand :: [String] -> IO ()
  execProductCommand (cmd:_) 
    | cmd == "add"      = showCreateProductForm 
    | otherwise         = showProductCommandUsage
  execProductCommand [] = showProductCommandUsage

  showCreateProductForm :: IO ()
  showCreateProductForm = do
    productName <- (putStrLn "Project Name: ") >> getLine
    P.createProduct productName

  showProductCommandUsage :: IO ()
  showProductCommandUsage = Paths.showProductCommandUsageFile

