{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API.Orders where

import Models
import Data.Aeson
import Servant
import Config

type OrderAPI =
    "markets" :> Capture "marketid" Int :> "outcomes" :> Capture "outcomeid" Int :> "orders" :> Get '[JSON] [Order] :<|>
    -- Should require authentication and authorization
    "markets" :> Capture "marketid" Int :> "outcomes" :> Capture "outcomeid" Int :> "orders" :> Capture "orderid" Int :> ReqBody '[JSON] Order :> Post '[JSON] Order :<|>
    "markets" :> Capture "marketid" Int :> "outcomes" :> Capture "outcomeid" Int :> "orders" :> Capture "orderid" Int :>  Get '[JSON] (Maybe Order)

orderServer :: ServerT OrderAPI App
orderServer = getOrders :<|> postOrder :<|> getOrder

getOrders :: Int -> Int -> App [Order]
getOrders _ _ = return []

postOrder :: Int -> Int -> Int -> Order -> App Order
postOrder _ _ _ = return

getOrder :: Int -> Int -> Int -> App (Maybe Order)
getOrder _ _ _ = return Nothing
