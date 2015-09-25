module Main where
  import Data.List (intersperse)
  import System.Environment (getArgs)

  data Command = Command { command :: String, arguments :: [String] } 
    deriving (Show)

  processCommand :: Command -> IO ()
  processCommand = undefined

  parseCommand :: [String] -> Maybe Command
  parseCommand [] = Nothing
  parseCommand (x:xs) = Just (Command x xs)

  main :: IO ()
  main = do
    putStrLn "Feature Creature!"
    args <- getArgs
    case (parseCommand args) of
      Nothing    -> putStrLn "Oh no! No arguments! Exiting..."
      (Just cmd) -> processCommand cmd
