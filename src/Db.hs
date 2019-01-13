{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Db where

import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|

  PurchaseOrder json
    isActive Bool
    deriving Show

  PurchaseOrderItem json
    isActive Bool
    purchaseOrderId PurchaseOrderId
    deriving Show

  PurchaseOrderItemLocation json
    isActive Bool
    purchaseOrderItemId PurchaseOrderItemId
    locationId Int
    quantity Int
    deriving Show


  Product json
    isActive Bool
    price Double
    deriving Show
|]

type Db = SqlPersistT IO 


connStr :: ConnectionString
connStr = "host=localhost dbname=orders user=postgres port=5432"


ensureDb :: ConnectionPool -> IO ()
ensureDb pool =
  flip runReaderT pool $ runDb $ runMigration migrateAll


runDb :: (MonadReader ConnectionPool m, MonadIO m) => Db b -> m b
runDb query = do
    pool <- ask
    liftIO $ runSqlPool query pool


selectPurchaseOrders :: Db [Entity PurchaseOrder]
selectPurchaseOrders =
  selectList [] []

findPurchaseOrder :: PurchaseOrderId -> Db (Maybe (Entity PurchaseOrder))
findPurchaseOrder poid = do
  getEntity poid

addPurchaseOrder :: PurchaseOrder -> Db PurchaseOrderId
addPurchaseOrder po =
  insert $ po

updatePurchaseOrder :: PurchaseOrderId -> PurchaseOrder -> Db ()
updatePurchaseOrder poid po =
  replace poid po


selectPurchaseOrderItems :: PurchaseOrderId -> Db [Entity PurchaseOrderItem]
selectPurchaseOrderItems poid =
  selectList [PurchaseOrderItemPurchaseOrderId ==. poid] []

findPurchaseOrderItem :: PurchaseOrderItemId -> Db (Maybe(Entity PurchaseOrderItem))
findPurchaseOrderItem poid =
  getEntity poid

addPurchaseOrderItem :: PurchaseOrderId -> PurchaseOrderItem -> Db PurchaseOrderItemId
addPurchaseOrderItem poid poi = do
  insert $ poi { purchaseOrderItemPurchaseOrderId = poid }

updatePurchaseOrderItem :: PurchaseOrderItemId -> PurchaseOrderItem -> Db ()
updatePurchaseOrderItem poiid poi =
  replace poiid poi


selectPurchaseOrderItemLocations :: PurchaseOrderItemId -> Db [Entity PurchaseOrderItemLocation]
selectPurchaseOrderItemLocations poiid =
  selectList [PurchaseOrderItemLocationPurchaseOrderItemId ==. poiid] []


selectProducts :: Db [Entity Product]
selectProducts =
  selectList [] []
