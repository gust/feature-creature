module CLI.ProductForm where
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
  showProductCommandUsage = putStrLn "showProductCommandUsage"

