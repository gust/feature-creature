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
  type Breadcrumb = FilePath

  instance ToJSON DirectoryTree where
    toJSON (Node fileDescription forest) =
      object [ "fileDescription" .= fileDescription
             , "forest"          .= forest
             ]

  instance ToJSON FileDescription where
    toJSON (FileDescription fName fPath) =
      object [ "fileName" .= fName
             , "filePath" .= fPath
             ]

  createNode :: FileDescription -> DirectoryTree
  createNode fd = Node fd []

  addToDirectoryTree :: DirectoryTree -> FilePath -> DirectoryTree
  addToDirectoryTree featureTree path =
    addToDirectoryTree' featureTree filePathParts initialBreadcrumb
      where
        filePathParts :: [T.Text]
        filePathParts = (splitFileName $ T.pack path)

        splitFileName :: T.Text -> [T.Text]
        splitFileName = (T.splitOn "/") . throwOutLeadingSlash

        throwOutLeadingSlash :: T.Text -> T.Text
        throwOutLeadingSlash txt
          | (T.head txt) == '/' = T.tail txt
          | otherwise           = txt

        initialBreadcrumb :: Breadcrumb
        initialBreadcrumb = ""

  addToDirectoryTree' :: DirectoryTree -> [T.Text] -> Breadcrumb -> DirectoryTree
  addToDirectoryTree' featureTree [] _ = featureTree
  addToDirectoryTree' (Node label forest) (directory:rest) path =
    let (matches, nonMatches) = partition (matchesLabel directory) forest
        newBreadcrumb         = path ++ "/" ++ (T.unpack directory) in
      case matches of
        [] -> do
          -- create a new node at the current depth in the Tree
          let newNode = createNode $ FileDescription { fileName = T.unpack directory, filePath = newBreadcrumb }
          Node label $ (addToDirectoryTree' newNode rest newBreadcrumb):forest
        [node] -> do
          -- add to an existing node at the current depth in the Tree by
          -- replacing the matching node with new content
          Node label $ (addToDirectoryTree' node rest newBreadcrumb):nonMatches
        (_:_) -> error "There should be a maximum of 1 match in the forest"

  matchesLabel :: T.Text -> DirectoryTree -> Bool
  matchesLabel file (Node fileDescription _) = (T.unpack file) == (fileName fileDescription)

