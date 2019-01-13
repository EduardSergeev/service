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
      Capture "id" PurchaseOrderId :> (
        Get '[JSON] (Entity PurchaseOrder) :<|>
        ReqBody '[JSON] PurchaseOrder :> PutNoContent '[JSON] () :<|>
        "Items" :> (
          Get '[JSON] [Entity PurchaseOrderItem] :<|>
          ReqBody '[JSON] PurchaseOrderItem :> Post '[JSON] (Entity PurchaseOrderItem)
        )
      )
    )
  )

server :: Server API
server =
  purchaseOrders
  where
    purchaseOrders =
      getPurchaseOrders :<|>
      postPurchaseOrder :<|>
      purchaseOrder
      where
        getPurchaseOrders =
          liftIO . runDb $ Db.selectPurchaseOrders

        postPurchaseOrder po =
          liftIO . runDb $ Db.addPurchaseOrder po

        purchaseOrder poid =
          getPurchaseOrder :<|>
          putPurchaseOrder :<|>
          purchaseOrderItems
          where
            getPurchaseOrder =
              liftIO . runDb $ Db.getPurchaseOrder poid

            putPurchaseOrder po =
              liftIO . runDb $ do
                Db.updatePurchaseOrder poid po

            purchaseOrderItems =
              getPurchaseOrderItems :<|>
              postPurchaseOrderItem
              where
                getPurchaseOrderItems =
                  liftIO . runDb $ Db.selectPurchaseOrderItems poid

                postPurchaseOrderItem poi =
                  liftIO . runDb $ Db.addPurchaseOrderItem poid poi


api :: Proxy API
api = Proxy

app :: Application
app = serve api server

startApp :: IO ()
startApp = run 8080 app
