{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where
  import Database.Persist.TH
  import Data.Text (Text)

  share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Product json sql=products
      name Text
      repoUrl Text
      deriving Show
  |]
