{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.DirectoryTree
  ( DirectoryTree
  , addToDirectoryTree
  , createNode
  ) where

  import Data.Aeson
  import Data.List (partition)
  import qualified Data.Text as T
  import Data.Tree (Tree(Node))

  type DirectoryTree = Tree FilePath

  instance ToJSON DirectoryTree where
    toJSON (Node label forest) = object ["label" .= label, "forest" .= forest]

  createNode :: FilePath -> DirectoryTree
  createNode file = Node file []

  addToDirectoryTree :: DirectoryTree -> FilePath -> DirectoryTree
  addToDirectoryTree featureTree filePath = addToDirectoryTree' featureTree (splitFileName $ T.pack filePath)
    where
      splitFileName :: T.Text -> [T.Text]
      splitFileName = (T.splitOn "/") . throwOutLeadingSlash 

      throwOutLeadingSlash :: T.Text -> T.Text
      throwOutLeadingSlash txt
        | (T.head txt) == '/' = T.tail txt
        | otherwise           = txt

  addToDirectoryTree' :: DirectoryTree -> [T.Text] -> DirectoryTree
  addToDirectoryTree' featureTree []                       = featureTree
  addToDirectoryTree' (Node label forest) [file]           = Node label $ forest ++ [(createNode $ T.unpack file)]
  addToDirectoryTree' (Node label forest) (directory:rest) =
    let (matches, nonMatches) = partition (matchesLabel directory) forest in
        case matches of
          [] -> do
            let newNode = createNode $ T.unpack directory
            Node label $ (addToDirectoryTree' newNode rest):forest
          [node] -> do
            Node label $ (addToDirectoryTree' node rest):nonMatches -- effectively replace the matching node
          (_:_) -> error "There should be a maximum of 1 match in the forest"

  matchesLabel :: T.Text -> DirectoryTree -> Bool
  matchesLabel file (Node label _) = (T.unpack file) == label

