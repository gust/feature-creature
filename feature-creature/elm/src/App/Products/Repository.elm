module App.Products.Repository exposing
  ( Repository
  , RepositoryMsg (..)
  , init
  )

import Http exposing (Error)

type alias Repository =
  { id   : Int
  , name : String
  }

type RepositoryMsg = FetchRepositoriesSucceeded (List Repository)
                   | FetchRepositoriesFailed Error

init : Int -> String -> Repository
init repID repName =
  { id   = repID
  , name = repName
  }
