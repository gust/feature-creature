module DomainTerms.DomainTerm
( findByProductId
, createDomainTerm
, updateDomainTerm
, removeDomainTerm
, findDomainTerm
, toDomainTermID
, toDomainTerm
, DomainTerm(..)
) where

import Control.Monad.Reader (ask, liftIO)
import Data.Int (Int64)
import qualified Database.Persist.Postgresql as DB
import Database.Types (WithDBPool (..))
import Models

createDomainTerm :: DomainTerm -> WithDBPool Int64
createDomainTerm domainTerm = ask
  >>= liftIO . (DB.runSqlPool (DB.insert domainTerm))
  >>= return . DB.fromSqlKey

updateDomainTerm :: DomainTermId -> DomainTerm -> WithDBPool DomainTerm
updateDomainTerm dtId domainTerm@(DomainTerm _ title description _) = ask
  >>= liftIO . (DB.runSqlPool updateDomainTermCommand)
  >> return domainTerm
  where
    updateDomainTermCommand =
      DB.update dtId [ DomainTermTitle DB.=. title
                     , DomainTermDescription DB.=. description
                     ]

removeDomainTerm :: ProductId -> DomainTermId -> WithDBPool ()
removeDomainTerm productID domainTermID = ask
  >>= liftIO . (DB.runSqlPool deleteDomainTermCommand)
  >>= return
  where
    deleteDomainTermCommand =
      DB.deleteWhere [ DomainTermProductId DB.==. productID
                     , DomainTermId DB.==. domainTermID
                     ]

findDomainTerm :: DomainTermId -> WithDBPool (Maybe DomainTerm)
findDomainTerm dtID = ask
  >>= liftIO . (DB.runSqlPool (DB.get dtID))

findByProductId :: ProductId -> WithDBPool [DB.Entity DomainTerm]
findByProductId productId = ask
  >>= liftIO . (DB.runSqlPool findByProductIdQuery)
  where
    findByProductIdQuery =
      DB.selectList
        [DomainTermProductId DB.==. productId]
        [ DB.Asc DomainTermTitle ]

toDomainTermID :: DB.Entity DomainTerm -> Int64
toDomainTermID dbEntity = DB.fromSqlKey . DB.entityKey $ dbEntity

toDomainTerm :: DB.Entity DomainTerm -> DomainTerm
toDomainTerm dbEntity = DB.entityVal dbEntity
