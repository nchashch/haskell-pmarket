{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API.Orders where

import Models.Order
import Models.Outcome
import Data.Aeson
import Servant
import GHC.Generics

type OrderAPI =
    "markets" :> Capture "marketid" Int :> "outcomes" :> Capture "outcomeid" Int :> "orders" :> Get '[JSON] [Order] :<|>
    -- Should require authentication and authorization
    "markets" :> Capture "marketid" Int :> "outcomes" :> Capture "outcomeid" Int :> "orders" :> Capture "orderid" Int :> ReqBody '[JSON] Order :> Post '[JSON] Order :<|>
    "markets" :> Capture "marketid" Int :> "outcomes" :> Capture "outcomeid" Int :> "orders" :> Capture "orderid" Int :>  Get '[JSON] Order

orderServer :: Server OrderAPI
orderServer = getOrders :<|> postOrder :<|> getOrder
    where
        getOrders :: Int -> Int -> Handler [Order]
        getOrders _ _ = return []

        postOrder :: Int -> Int -> Int -> Order -> Handler Order
        postOrder _ _ _ = return

        getOrder :: Int -> Int -> Int -> Handler Order
        getOrder _ _ _ = return Order {
          _orderId = 1,
          _outcome = OutcomeId 1,
          _amount = 100
                                      }
