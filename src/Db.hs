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

import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger    (runStderrLoggingT)
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


connStr :: ConnectionString
connStr = "host=localhost dbname=orders user=postgres port=5432"

ensureDb :: IO ()
ensureDb =
  runDb $ runMigration migrateAll

runDb :: SqlPersistT IO b -> IO b
runDb query =
  runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $
    flip runSqlPool pool $ query


selectPurchaseOrders :: SqlPersistT IO [Entity PurchaseOrder]
selectPurchaseOrders =
  selectList [] []

findPurchaseOrder :: PurchaseOrderId -> SqlPersistT IO (Entity PurchaseOrder)
findPurchaseOrder poid = do
  [po] <- selectList [PurchaseOrderId ==. poid] [LimitTo 1]
  return po

addPurchaseOrder :: PurchaseOrder -> SqlPersistT IO PurchaseOrderId
addPurchaseOrder po =
  insert $ po

updatePurchaseOrder :: PurchaseOrderId -> PurchaseOrder -> SqlPersistT IO ()
updatePurchaseOrder poid po =
  replace poid po


selectPurchaseOrderItems :: PurchaseOrderId -> SqlPersistT IO [Entity PurchaseOrderItem]
selectPurchaseOrderItems poid =
  selectList [PurchaseOrderItemPurchaseOrderId ==. poid] []

findPurchaseOrderItem :: PurchaseOrderItemId -> SqlPersistT IO (Entity PurchaseOrderItem)
findPurchaseOrderItem poid = do
  [poi] <- selectList [PurchaseOrderItemId ==. poid] [LimitTo 1]
  return poi

addPurchaseOrderItem :: PurchaseOrderId -> PurchaseOrderItem -> SqlPersistT IO PurchaseOrderItemId
addPurchaseOrderItem poid poi = do
  insert $ poi { purchaseOrderItemPurchaseOrderId = poid }

updatePurchaseOrderItem :: PurchaseOrderItemId -> PurchaseOrderItem -> SqlPersistT IO ()
updatePurchaseOrderItem poiid poi =
  replace poiid poi


selectPurchaseOrderItemLocations :: PurchaseOrderItemId -> SqlPersistT IO [Entity PurchaseOrderItemLocation]
selectPurchaseOrderItemLocations poiid =
  selectList [PurchaseOrderItemLocationPurchaseOrderItemId ==. poiid] []


selectProducts :: SqlPersistT IO [Entity Product]
selectProducts =
  selectList [] []
