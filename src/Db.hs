{-# LANGUAGE DeriveGeneric              #-}
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

import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader(..), runReaderT)
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.List (groupBy)
import qualified Database.Esqueleto as E
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           GHC.Generics (Generic)


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

data PurchaseOrderNav = PurchaseOrderNav {
    purchaseOrder :: Entity PurchaseOrder
  , purchaseOrderItems' :: [Entity PurchaseOrderItem]
  } deriving Generic

instance ToJSON PurchaseOrderNav where
  toJSON (PurchaseOrderNav po pois) =
    case toJSON po of
      Object o -> Object $ HM.insert "items" (toJSON pois) o
      x -> x

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

selectPurchaseOrdersAndItems :: Db [PurchaseOrderNav]
selectPurchaseOrdersAndItems = do
  pos <- E.select $
    E.from $ \po -> do
    E.orderBy [E.asc (po E.^. PurchaseOrderId)]
    return po
  pois <- E.select $
    E.from $ \(po, poi) -> do
    E.where_ (poi E.^. PurchaseOrderItemPurchaseOrderId E.==. po E.^. PurchaseOrderId)
    E.orderBy [E.asc (poi E.^. PurchaseOrderItemPurchaseOrderId)]
    return poi
  return (merge pos . groupBy (\a b -> byId a == byId b) $ pois)
  where
    byId = purchaseOrderItemPurchaseOrderId . entityVal
    merge pos [] =
      fmap (`PurchaseOrderNav` []) pos 
    merge (po:pos') poiss@(pois@(poi:_):poiss')
      | entityKey po == byId poi =
          PurchaseOrderNav po pois : merge pos' poiss'
      | otherwise =
          PurchaseOrderNav po [] : merge pos' poiss

findPurchaseOrder :: PurchaseOrderId -> Db (Maybe (Entity PurchaseOrder))
findPurchaseOrder poid =
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
