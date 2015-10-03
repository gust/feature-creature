module CLI.ProductForm where
  import qualified CLI.DataFiles as Paths
  import Data.Text (pack)
  import Products.Product

  execProductCommand :: [String] -> IO ()
  execProductCommand (cmd:_) 
    | cmd == "add"      = showCreateProductForm 
    | otherwise         = showProductCommandUsage
  execProductCommand [] = showProductCommandUsage

  showCreateProductForm :: IO ()
  showCreateProductForm = do
    productName <- (putStrLn "Project Name: ") >> getLine
    id <- createProduct $ Product (pack productName)
    putStrLn $ "Product " ++ (show id) ++ " created!"

  showProductCommandUsage :: IO ()
  showProductCommandUsage = Paths.showProductCommandUsageFile

