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
  import qualified Database.Persist.Postgresql as DB

  type Entity = DB.Entity

  share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Product json sql=products
      name Text
      repoUrl Text
      deriving Show

    UserRole json sql=user_roles
      productId ProductId
      title Text
      description Text
      deriving Show

    DomainTerm json sql=domain_terms
      productId ProductId
      title Text
      description Text
      deriving Show
  |]

  toKey :: (Integral i, ToBackendKey SqlBackend record) => i -> Key record
  toKey = toSqlKey . fromIntegral
