{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where

import Database.Persist.TH
import Database.Persist.Sql(toSqlKey,SqlBackend,ToBackendKey,Key(..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Database.Persist.Postgresql as DB
import ModelTypes

type Entity = DB.Entity

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Product json sql=products
    name Text sqltype=text
    repoUrl Text sqltype=text
    created UTCTime default=now()
    deriving Show

  RepositoryStatus json sql=repository_status
    productId ProductId
    state RepositoryState
    error Text Maybe default=Nothing sqltype=text
    created UTCTime default=now()
    UniqueProductID productId
    deriving Show

  UserRole json sql=user_roles
    productId ProductId
    title Text sqltype=text
    description Text sqltype=text
    created UTCTime default=now()
    deriving Show

  DomainTerm json sql=domain_terms
    productId ProductId
    title Text sqltype=text
    description Text sqltype=text
    created UTCTime default=now()
    deriving Show
|]

toKey :: (Integral i, ToBackendKey SqlBackend record) => i -> Key record
toKey = toSqlKey . fromIntegral
