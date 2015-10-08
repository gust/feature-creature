module CLI.ProductForm where
  import qualified CLI.DataFiles as Paths
  import Data.List (intersperse)
  import Data.Text (pack)
  import Products.Product

  execProductCommand :: [String] -> IO ()
  execProductCommand (cmd:_) 
    | cmd == "list"     = listAllProducts
    | cmd == "add"      = showCreateProductForm 
    | otherwise         = showProductCommandUsage
  execProductCommand [] = showProductCommandUsage

  showCreateProductForm :: IO ()
  showCreateProductForm = do
    prodName <- (putStrLn "Project Name: ") >> getLine
    prodId <- createProduct $ Product (pack prodName)
    putStrLn $ "Product " ++ (show prodId) ++ " created!"

  showProductCommandUsage :: IO ()
  showProductCommandUsage = Paths.showProductCommandUsageFile

  listAllProducts :: IO ()
  listAllProducts = do
    prods <- findProducts
    putStrLn $ concat . (intersperse "\n") $ map show prods
