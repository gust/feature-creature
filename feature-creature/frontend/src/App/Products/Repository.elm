module App.Products.Repository exposing
  ( Repository
  , RepositoryMsg (..)
  , RepositoryOwner
  , getRepositories
  )

import App.AppConfig exposing (..)
import Http as Http exposing (..)
import Json.Decode as Json  exposing ((:=), maybe)
import Task as Task exposing (..)

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

getRepositories : AppConfig -> Cmd RepositoryMsg
getRepositories appConfig =
  Http.get parseRepositories (repositoriesUrl appConfig)
    |> Task.perform FetchRepositoriesFailed FetchRepositoriesSucceeded

parseRepositories : Json.Decoder (List Repository)
parseRepositories = parseRepository |> Json.list

parseRepository : Json.Decoder Repository
parseRepository =
  Json.object8
    Repository
      ("id"   := Json.int)
      ("name" := Json.string)
      ("url" := Json.string)
      ("htmlUrl" := Json.string)
      (maybe ("sshUrl" := Json.string))
      (maybe ("cloneUrl" := Json.string))
      ("hooksUrl" := Json.string)
      ("owner" := parseRepositoryOwner)

parseRepositoryOwner : Json.Decoder RepositoryOwner
parseRepositoryOwner =
  Json.object4
    RepositoryOwner
      ("id"   := Json.int)
      ("name" := Json.string)
      ("url" := Json.string)
      ("avatarUrl" := Json.string)

repositoriesUrl : AppConfig -> String
repositoriesUrl appConfig = appConfig.productsApiPath ++ "/api/repositories"
