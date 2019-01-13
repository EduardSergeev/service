{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( startApp
    , app
    ) where

import Control.Monad.IO.Class (liftIO)
import Database.Persist.Types (Entity)
import Db
import Network.Wai
import Network.Wai.Handler.Warp
import Servant


type API =
  "api" :> (
    "PurchaseOrders" :> (
      Get '[JSON] [Entity PurchaseOrder] :<|>
      ReqBody '[JSON] PurchaseOrder :> Post '[JSON] PurchaseOrderId :<|>
      Capture "poid" PurchaseOrderId :> (
        Get '[JSON] (Entity PurchaseOrder) :<|>
        ReqBody '[JSON] PurchaseOrder :> PutNoContent '[JSON] () :<|>
        "Items" :> (
          Get '[JSON] [Entity PurchaseOrderItem] :<|>
          ReqBody '[JSON] PurchaseOrderItem :> Post '[JSON] PurchaseOrderItemId :<|>
          Capture "poiid" PurchaseOrderItemId :> (
            Get '[JSON] (Entity PurchaseOrderItem) :<|>
            ReqBody '[JSON] PurchaseOrderItem :> PutNoContent '[JSON] () :<|>
            "Locations" :> (
              Get '[JSON] [Entity PurchaseOrderItemLocation]
            )
          )
        )
      )
    ) :<|>
    "Products" :> (
      Get '[JSON] [Entity Product]
    )
  )

server :: Server API
server =
  purchaseOrders :<|>
  products
  where
    purchaseOrders =
      getPurchaseOrders :<|>
      postPurchaseOrder :<|>
      withPurchaseOrder
      where
        withPurchaseOrder poid =
          getPurchaseOrder poid :<|>
          putPurchaseOrder poid :<|>
          purchaseOrderItems
          where
            purchaseOrderItems =
              getPurchaseOrderItems poid :<|>
              postPurchaseOrderItem poid :<|>
              withPurchaseOrderItem
              where
                withPurchaseOrderItem poiid =
                  getPurchaseOrderItem poiid :<|>
                  putPurchaseOrderItem poiid :<|>
                  purchaseOrderItemLocations
                  where
                    purchaseOrderItemLocations =
                      getPurchaseOrderItemLocations poiid
    products =
      getProducts


getPurchaseOrders :: Handler [Entity PurchaseOrder]
getPurchaseOrders =
  liftIO . runDb $ selectPurchaseOrders

getPurchaseOrder :: PurchaseOrderId -> Handler (Entity PurchaseOrder)
getPurchaseOrder =
  liftIO . runDb . findPurchaseOrder

postPurchaseOrder :: PurchaseOrder -> Handler PurchaseOrderId
postPurchaseOrder =
  liftIO . runDb . addPurchaseOrder

putPurchaseOrder :: PurchaseOrderId -> PurchaseOrder -> Handler ()
putPurchaseOrder poid =
  liftIO . runDb . updatePurchaseOrder poid


getPurchaseOrderItems :: PurchaseOrderId -> Handler [Entity PurchaseOrderItem]
getPurchaseOrderItems =
  liftIO . runDb . selectPurchaseOrderItems

getPurchaseOrderItem :: PurchaseOrderItemId -> Handler (Entity PurchaseOrderItem)
getPurchaseOrderItem =
  liftIO . runDb . findPurchaseOrderItem

postPurchaseOrderItem :: PurchaseOrderId -> PurchaseOrderItem -> Handler PurchaseOrderItemId
postPurchaseOrderItem poid =
  liftIO . runDb . addPurchaseOrderItem poid

putPurchaseOrderItem :: PurchaseOrderItemId -> PurchaseOrderItem -> Handler ()
putPurchaseOrderItem poiid =
  liftIO . runDb . updatePurchaseOrderItem poiid

getPurchaseOrderItemLocations :: PurchaseOrderItemId -> Handler [Entity PurchaseOrderItemLocation]
getPurchaseOrderItemLocations =
  liftIO . runDb . selectPurchaseOrderItemLocations


getProducts :: Handler [Entity Product]
getProducts =
  liftIO . runDb $ Db.selectProducts


api :: Proxy API
api = Proxy

app :: Application
app = serve api server

startApp :: IO ()
startApp = run 8080 app
