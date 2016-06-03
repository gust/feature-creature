{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.DirectoryTree
( DirectoryTree (..)
, FileDescription (..)
, addToDirectoryTree
, createEmptyTree
) where

import Control.Monad (mzero)
import Data.Aeson
import Data.List (partition)
import qualified Data.Text as T
import Data.Tree (Tree(Node))

data FileDescription =
  FileDescription { fileName :: String
                  , filePath :: FilePath
                  } deriving (Show, Eq)

type Breadcrumb = FilePath

newtype DirectoryTree = DirectoryTree { unTree :: Tree FileDescription }
  deriving (Show, Eq)

instance ToJSON DirectoryTree where
  toJSON (DirectoryTree (Node fileDescription forest)) =
    object [ "fileDescription" .= fileDescription
           , "forest"          .= fmap DirectoryTree forest
           ]

instance FromJSON DirectoryTree where
  parseJSON = withObject "directoryTree" $ \o -> do
    fileDescription <- o .: "fileDescription"
    forest          <- o .: "forest"
    return $ DirectoryTree $ Node fileDescription (fmap unTree forest)

instance ToJSON FileDescription where
  toJSON (FileDescription fName fPath) =
    object [ "fileName" .= fName
           , "filePath" .= fPath
           ]

instance FromJSON FileDescription where
  parseJSON (Object v) = FileDescription <$>
                        v .: "fileName" <*>
                        v .: "filePath"
  parseJSON _          = mzero

createEmptyTree :: DirectoryTree
createEmptyTree = DirectoryTree $ createNode $ FileDescription "/" "/"

createNode :: FileDescription -> Tree FileDescription
createNode fd = Node fd []

addToDirectoryTree :: DirectoryTree -> FilePath -> DirectoryTree
addToDirectoryTree (DirectoryTree featureTree) path =
  DirectoryTree $ addToDirectoryTree' featureTree filePathParts initialBreadcrumb
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

addToDirectoryTree' :: Tree FileDescription -> [T.Text] -> Breadcrumb -> Tree FileDescription
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

matchesLabel :: T.Text -> Tree FileDescription -> Bool
matchesLabel file (Node fileDescription _) = (T.unpack file) == (fileName fileDescription)

