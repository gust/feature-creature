module App.Products.Repository exposing
  ( Repository
  , RepositoryMsg (..)
  , RepositoryOwner
  )

import Http exposing (Error)

type alias Repository =
  { id       : Int
  , name     : String
  , url      : String
  , htmlUrl  : String
  , sshUrl   : Maybe String
  , cloneUrl : Maybe String
  , hooksUrl : String
  , owner    : RepositoryOwner
  }

type alias RepositoryOwner =
  { id        : Int
  , name      : String
  , url       : String
  , avatarUrl : String
  }

type RepositoryMsg = FetchRepositoriesSucceeded (List Repository)
                   | FetchRepositoriesFailed Error
