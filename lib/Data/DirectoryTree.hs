{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.DirectoryTree
  ( DirectoryTree
  , FileDescription (..)
  , addToDirectoryTree
  , createNode
  ) where

  import Data.Aeson
  import Data.List (partition)
  import qualified Data.Text as T
  import Data.Tree (Tree(Node))

  data FileDescription =
    FileDescription { fileName :: String
                    , filePath :: FilePath
                    } deriving (Show)
  type DirectoryTree = Tree FileDescription

  type Cursor = FilePath

  instance ToJSON DirectoryTree where
    toJSON (Node fileDescription forest) =
      object [ "fileDescription" .= fileDescription
             , "forest"          .= forest
             ]

  instance ToJSON FileDescription where
    toJSON (FileDescription fileName filePath) =
      object [ "fileName" .= fileName
             , "filePath" .= filePath
             ]

  createNode :: FileDescription -> DirectoryTree
  createNode fd = Node fd []

  addToDirectoryTree :: DirectoryTree -> FilePath -> DirectoryTree
  addToDirectoryTree featureTree filePath =
    addToDirectoryTree' featureTree filePathParts initialCursor
      where
        filePathParts :: [T.Text]
        filePathParts = (splitFileName $ T.pack filePath)

        splitFileName :: T.Text -> [T.Text]
        splitFileName = (T.splitOn "/") . throwOutLeadingSlash

        throwOutLeadingSlash :: T.Text -> T.Text
        throwOutLeadingSlash txt
          | (T.head txt) == '/' = T.tail txt
          | otherwise           = txt

        initialCursor :: Cursor
        initialCursor = ""

  addToDirectoryTree' :: DirectoryTree -> [T.Text] -> Cursor -> DirectoryTree
  addToDirectoryTree' featureTree [] _                                 = featureTree
  addToDirectoryTree' (Node label forest) (directory:rest) path =
    let (matches, nonMatches) = partition (matchesLabel directory) forest in
        case matches of
          [] -> do
            let newPath           = path ++ "/" ++ (T.unpack directory)
            let newCursor         = newPath
            let newFileDescripton = FileDescription { fileName = T.unpack directory, filePath = newPath }
            let newNode           = createNode newFileDescripton
            Node label $ (addToDirectoryTree' newNode rest newCursor):forest
          [node] -> do
            let newPath = path ++ "/" ++ (T.unpack directory)
            let newCursor = newPath
            Node label $ (addToDirectoryTree' node rest newCursor):nonMatches -- effectively replace the matching node
          (_:_) -> error "There should be a maximum of 1 match in the forest"

  matchesLabel :: T.Text -> DirectoryTree -> Bool
  matchesLabel file (Node fileDescription _) = (T.unpack file) == (fileName fileDescription)

