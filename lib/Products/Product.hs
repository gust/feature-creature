module Products.Product where

  createProduct :: String -> IO ()
  createProduct name = putStrLn $ "Project '" ++ name ++ "' created."
