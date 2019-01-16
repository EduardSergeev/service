{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Db where

import           Control.Lens
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader(..), runReaderT)
import           Data.List (groupBy)
import           Data.Maybe (catMaybes)
import qualified Database.Esqueleto as E
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH


share [mkPersist sqlSettings { mpsGenerateNavigationProperties = True, mpsGenerateLenses = True }, mkMigrate "migrateAll"] [persistUpperCase|

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
    productItemId Int
    locationId Int
    quantity Int
    UniqueItemLocation productItemId locationId
    deriving Show


  Product json
    isActive Bool
    price Double
    deriving Show
|]

makeLensesFor [("entityKey","_entityKey"),("entityVal", "_entityVal")] ''Entity

type Db = SqlPersistT IO 


runDb :: (MonadReader ConnectionPool m, MonadIO m) => Db b -> m b
runDb query = do
  pool <- ask
  liftIO $ runSqlPool query pool


ensureDb :: ConnectionPool -> IO ()
ensureDb pool =
  flip runReaderT pool $ runDb $ runMigration migrateAll


selectPurchaseOrders :: Db [Entity PurchaseOrder]
selectPurchaseOrders =
  selectList [] []

selectPurchaseOrdersAndItems :: Db [Entity PurchaseOrder]
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
    byId = _purchaseOrderItemPurchaseOrderId . entityVal
    merge pos [] =
      pos 
    merge (po:pos') poiss@(pois@(poi:_):poiss')
      | entityKey po == byId poi =
          (po & _entityVal . purchaseOrderPurchaseOrderItems .~ pois) : merge pos' poiss'
      | otherwise =
          po : merge pos' poiss
    merge _ _ = error "DB corrupted"

selectPurchaseOrdersAndItems4 :: Db [(Entity PurchaseOrder)]
selectPurchaseOrdersAndItems4 =
  selectWithChildren
    PurchaseOrderId
    PurchaseOrderItemPurchaseOrderId
    purchaseOrderPurchaseOrderItems
    (\_ -> E.val True)


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
  selectWithChildren
    PurchaseOrderItemId
    PurchaseOrderItemLocationPurchaseOrderItemId
    purchaseOrderItemPurchaseOrderItemLocations
    (\poi -> poi E.^. PurchaseOrderItemPurchaseOrderId E.==. E.val poid)


findPurchaseOrderItem :: PurchaseOrderItemId -> Db (Maybe(Entity PurchaseOrderItem))
findPurchaseOrderItem poid =
  getEntity poid

addPurchaseOrderItem :: PurchaseOrderId -> PurchaseOrderItem -> Db PurchaseOrderItemId
addPurchaseOrderItem poid poi = do
  insert $ poi { _purchaseOrderItemPurchaseOrderId = poid }

updatePurchaseOrderItem :: PurchaseOrderItemId -> PurchaseOrderItem -> Db ()
updatePurchaseOrderItem poiid poi =
  replace poiid poi


selectPurchaseOrderItemLocations :: PurchaseOrderItemId -> Db [Entity PurchaseOrderItemLocation]
selectPurchaseOrderItemLocations poiid =
  selectList [PurchaseOrderItemLocationPurchaseOrderItemId ==. poiid] []

addPurchaseOrderItemLocation :: PurchaseOrderItemId -> PurchaseOrderItemLocation -> Db PurchaseOrderItemLocationId
addPurchaseOrderItemLocation poiid poil = do
  insert $ poil { _purchaseOrderItemLocationPurchaseOrderItemId = poiid }

selectProducts :: Db [Entity Product]
selectProducts =
  selectList [] []


selectWithChildren ::
  (PersistEntity p, PersistEntity c, BackendCompatible SqlBackend (PersistEntityBackend p), BackendCompatible SqlBackend (PersistEntityBackend c)) =>
  (EntityField p (Key p)) ->
  (EntityField c (Key p)) ->
  (forall f. Functor f =>
      ([Entity c] -> f [Entity c])
      -> p -> f p) ->
  (E.SqlExpr (Entity p) -> E.SqlExpr (E.Value Bool)) ->
  Db [Entity p]
selectWithChildren pid cid is wp = do
  pps <- E.select $
    E.from $ \(po `E.LeftOuterJoin` poi) -> do
    E.on (E.just (po E.^. pid) E.==. poi E.?. cid)
    E.where_ (wp po)
    E.orderBy [E.asc (po E.^. pid)]
    return (po, poi)
  return . fmap (\pps'@((po,_):_) -> po & _entityVal . is .~ (catMaybes . fmap snd $ pps')) . groupBy (\a b -> byFn a == byFn b) $ pps
  where
    byFn = entityKey . fst
