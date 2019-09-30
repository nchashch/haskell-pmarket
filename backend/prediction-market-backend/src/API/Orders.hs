{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API.Orders where

import Config
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Models
import Servant

-- Should require authentication and authorization
type OrderAPI =
    "orders" :> Get '[JSON] [Entity Order] :<|>
    "markets" :> Capture "marketid" MarketId :> "orders" :> Get '[JSON] [Entity Order] :<|>
    "markets" :> Capture "marketid" MarketId :> "outcomes" :> Capture "outcome_index" OutcomeIndex :> "orders" :> Get '[JSON] [Entity Order] :<|>
    "orders" :> Capture "orderid" OrderId :> Get '[JSON] (Entity Order) :<|>
    "orders" :> ReqBody '[JSON] Order :> Post '[JSON] OrderId

orderServer :: ServerT OrderAPI App
orderServer = getOrders :<|> getMarketOrders :<|> getOutcomeOrders :<|> getOrder :<|> postOrder

getOrders :: App [Entity Order]
getOrders = runDb $ selectList [] []

getMarketOrders :: MarketId -> App [Entity Order]
getMarketOrders marketId = runDb $ selectList [OrderMarketId ==. marketId] []

getOutcomeOrders :: MarketId -> OutcomeIndex -> App [Entity Order]
getOutcomeOrders marketId outcomeIndex = runDb $
  selectList
  [
    OrderMarketId ==. marketId,
    OrderOutcomeIndex ==. outcomeIndex
  ]
  []

getOrder :: OrderId -> App (Entity Order)
getOrder orderId = do
  order <- runDb $ getEntity orderId
  maybe (throwError err404) return order

postOrder :: Order -> App OrderId
postOrder order = runDb $ do
  orderEntity <- insertEntity order
  executeOrder order
  return $ entityKey orderEntity

executeOrder :: Order -> SqlPersistT IO (Entity Position)
executeOrder order = do
    updateOutcome marketId outcomeIndex amount
    upsertPosition marketId outcomeIndex amount
  where
    marketId = orderMarketId order
    outcomeIndex = orderOutcomeIndex order
    amount = orderAmount order

upsertPosition :: MarketId -> OutcomeIndex -> OutcomeAmount -> SqlPersistT IO (Entity Position)
upsertPosition marketId outcomeIndex amount =
  upsertBy
    (PrimaryPosition marketId outcomeIndex)
    position
    [PositionAmount +=. amount]
  where
    position = Position {
    positionMarketId = marketId,
    positionOutcomeIndex = outcomeIndex,
    positionAmount = amount
                        }

updateOutcome :: MarketId -> OutcomeIndex -> OutcomeAmount -> SqlPersistT IO ()
updateOutcome marketId outcomeIndex amount =
  updateBy (PrimaryOutcome marketId outcomeIndex) [OutcomeOutstanding +=. amount]
