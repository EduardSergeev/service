{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Lib (
    startApp,
    app
  ) where


import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (ReaderT(..))
import Database.Persist.Postgresql (ConnectionPool, withPostgresqlPool)
import Database.Persist.Types (Entity)
import Db
import Network.Wai.Handler.Warp (run)
import Servant

type App = ReaderT ConnectionPool Handler

type ServerApp api = ServerT api App

type API =
  "api" :> PurchaseOrderAPI :<|> ProductAPI

type PurchaseOrderAPI = 
  "PurchaseOrders" :>
    Get '[JSON] [Entity PurchaseOrder] :<|>
    "Items" :> Get '[JSON] [PurchaseOrderNav] :<|>
    ReqBody '[JSON] PurchaseOrder :> Post '[JSON] PurchaseOrderId :<|>
    Capture "poid" PurchaseOrderId :> (
      Get '[JSON] (Entity PurchaseOrder) :<|>
      ReqBody '[JSON] PurchaseOrder :> PutNoContent '[JSON] () :<|>
      PurchaseOrdeItemAPI
    )
  
type PurchaseOrdeItemAPI =
  "Items" :>
    Get '[JSON] [Entity PurchaseOrderItem] :<|>
    ReqBody '[JSON] PurchaseOrderItem :> Post '[JSON] PurchaseOrderItemId :<|>
    Capture "poiid" PurchaseOrderItemId :> (
      Get '[JSON] (Entity PurchaseOrderItem) :<|>
      ReqBody '[JSON] PurchaseOrderItem :> PutNoContent '[JSON] () :<|>
      PurchaseOrdeItemLocationAPI
    )

type PurchaseOrdeItemLocationAPI =
  "Locations" :>
    Get '[JSON] [Entity PurchaseOrderItemLocation]

  
type ProductAPI =
  "Products" :> (
    Get '[JSON] [Entity Product]
  )

 
serverT :: ServerApp API
serverT =
  purchaseOrders :<|>
  products

purchaseOrders :: ServerApp PurchaseOrderAPI
purchaseOrders =
  getPurchaseOrders :<|>
  getPurchaseOrdersAndItems :<|>
  postPurchaseOrder :<|>
  withPurchaseOrder
  where
    withPurchaseOrder poid =
      getPurchaseOrder poid :<|>
      putPurchaseOrder poid :<|>
      purchaseOrderItems poid

purchaseOrderItems :: PurchaseOrderId -> ServerApp PurchaseOrdeItemAPI
purchaseOrderItems poid =
  getPurchaseOrderItems poid :<|>
  postPurchaseOrderItem poid :<|>
  withPurchaseOrderItem
  where
    withPurchaseOrderItem poiid =
      getPurchaseOrderItem poiid :<|>
      putPurchaseOrderItem poiid :<|>
      purchaseOrderItemLocations poiid

purchaseOrderItemLocations :: PurchaseOrderItemId -> ServerApp PurchaseOrdeItemLocationAPI
purchaseOrderItemLocations poiid =
  getPurchaseOrderItemLocations poiid


products :: ServerT ProductAPI App
products =
  getProducts


getPurchaseOrders :: App [Entity PurchaseOrder]
getPurchaseOrders =
  runDb $ selectPurchaseOrders

getPurchaseOrdersAndItems :: App [PurchaseOrderNav]
getPurchaseOrdersAndItems =
  runDb $ selectPurchaseOrdersAndItems

getPurchaseOrder :: PurchaseOrderId -> App (Entity PurchaseOrder)
getPurchaseOrder =
  throwError err404 `maybe` return <=< runDb . findPurchaseOrder

postPurchaseOrder :: PurchaseOrder -> App PurchaseOrderId
postPurchaseOrder =
  runDb . addPurchaseOrder

putPurchaseOrder :: PurchaseOrderId -> PurchaseOrder -> App ()
putPurchaseOrder poid =
  runDb . updatePurchaseOrder poid


getPurchaseOrderItems :: PurchaseOrderId -> App [Entity PurchaseOrderItem]
getPurchaseOrderItems =
  runDb . selectPurchaseOrderItems

getPurchaseOrderItem :: PurchaseOrderItemId -> App (Entity PurchaseOrderItem)
getPurchaseOrderItem =
  throwError err404 `maybe` return <=< runDb . findPurchaseOrderItem

postPurchaseOrderItem :: PurchaseOrderId -> PurchaseOrderItem -> App PurchaseOrderItemId
postPurchaseOrderItem poid =
  runDb . addPurchaseOrderItem poid

putPurchaseOrderItem :: PurchaseOrderItemId -> PurchaseOrderItem -> App ()
putPurchaseOrderItem poiid =
  runDb . updatePurchaseOrderItem poiid

getPurchaseOrderItemLocations :: PurchaseOrderItemId -> App [Entity PurchaseOrderItemLocation]
getPurchaseOrderItemLocations =
  runDb . selectPurchaseOrderItemLocations


getProducts :: App [Entity Product]
getProducts =
  runDb $ Db.selectProducts


api :: Proxy API
api = Proxy

server :: ConnectionPool -> Server API
server pool = hoistServer api (flip runReaderT pool) serverT

app :: ConnectionPool -> Application
app pool = serve api $ server pool

startApp :: IO ()
startApp =
  runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    ensureDb pool
    run 8080 $ app pool
