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

import Database.Persist
import Database.Persist.TH
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist.Postgresql


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

getPurchaseOrder :: PurchaseOrderId -> SqlPersistT IO (Entity PurchaseOrder)
getPurchaseOrder poid = do
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

getPurchaseOrderItem :: PurchaseOrderItemId -> SqlPersistT IO (Entity PurchaseOrderItem)
getPurchaseOrderItem poid = do
  [poi] <- selectList [PurchaseOrderItemId ==. poid] [LimitTo 1]
  return poi

addPurchaseOrderItem :: PurchaseOrderId -> PurchaseOrderItem -> SqlPersistT IO (Entity PurchaseOrderItem)
addPurchaseOrderItem poid poi = do
  poiid <- insert $ poi { purchaseOrderItemPurchaseOrderId = poid }
  getPurchaseOrderItem poiid 
