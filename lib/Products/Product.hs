module Products.Product where
  import Data.List (intersperse)

  createProduct :: String -> IO ()
  createProduct name = putStrLn $ "Project '" ++ name ++ "' created."
