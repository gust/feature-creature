{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where
  import Database.Persist.TH
  import Data.Time

  share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Product json sql=products
      name String
      createdAt UTCTime default=now()
      updatedAt UTCTime default=now()
      deriving Show
  |]
