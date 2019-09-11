{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module API (
    PredictionMarketAPI,
    predictionMarketServer,
    predictionMarket
    ) where

import Data.Aeson
import Servant
import GHC.Generics

type PredictionMarketAPI =
    ResolutionAPI :<|>
    MarketAPI :<|>
    OutcomeAPI :<|>
    PositionAPI :<|>
    OrderAPI

predictionMarketServer :: Server PredictionMarketAPI
predictionMarketServer =
    resolutionServer :<|>
    marketServer :<|>
    outcomeServer :<|>
    positionServer :<|>
    orderServer

predictionMarketAPI :: Proxy PredictionMarketAPI
predictionMarketAPI = Proxy

predictionMarket :: Application
predictionMarket = serve predictionMarketAPI predictionMarketServer

data Market = Market deriving Generic
instance FromJSON Market
instance ToJSON Market

data Outcome = Outcome deriving Generic
instance FromJSON Outcome
instance ToJSON Outcome

data Position = Position deriving Generic
instance FromJSON Position
instance ToJSON Position

data Order = Order deriving Generic
instance FromJSON Order
instance ToJSON Order

data Resolution =
    Resolution {
        resolutionId :: Int,
        marketId :: Int,
        outcomeId :: Int
    } deriving Generic
instance FromJSON Resolution
instance ToJSON Resolution

type ResolutionAPI =
    "resolutions" :> Get '[JSON] [Resolution] :<|>
    "resolutions" :> ReqBody '[JSON] Resolution :> Post '[JSON] Resolution :<|>
    "resolutions" :> Capture "resolutionid" Int :> Get '[JSON] Resolution

resolutionServer :: Server ResolutionAPI
resolutionServer = getResolutions :<|> postResolution :<|> getResolution
    where
        getResolutions :: Handler [Resolution]
        getResolutions = return []

        postResolution :: Resolution -> Handler Resolution
        postResolution = return

        getResolution :: Int -> Handler Resolution
        getResolution _ = return $ Resolution 0 0 0

type MarketAPI =
    "markets" :> Get '[JSON] [Market] :<|>
    -- Should require authentication and authorization
    "markets" :> ReqBody '[JSON] Market :> Post '[JSON] Market :<|>
    "markets" :> Capture "marketid" Int :> Get '[JSON] Market

marketServer :: Server MarketAPI
marketServer = getMarkets :<|> postMarket :<|> getMarket
    where
        getMarkets :: Handler [Market]
        getMarkets = return []

        postMarket :: Market -> Handler Market
        postMarket = return

        getMarket :: Int -> Handler Market
        getMarket _ = return Market

type OutcomeAPI =
    "markets" :> Capture "marketid" Int :> "outcomes" :> Get '[JSON] [Outcome] :<|>
    "markets" :> Capture "marketid" Int :> "outcomes" :> Capture "outcomeid" Int :> Get '[JSON] Outcome

outcomeServer :: Server OutcomeAPI
outcomeServer = getOutcomes :<|> getOutcome
    where
        getOutcomes :: Int -> Handler [Outcome]
        getOutcomes _ = return []

        getOutcome :: Int -> Int -> Handler Outcome
        getOutcome _ _ = return Outcome

type PositionAPI =
    "markets" :> Capture "marketid" Int :> "positions" :> Get '[JSON] [Position] :<|>
    "markets" :> Capture "marketid" Int :> "positions" :> Capture "positionid" Int :> Get '[JSON] Position

positionServer :: Server PositionAPI
positionServer = getPositions :<|> getPosition
    where
        getPositions :: Int -> Handler [Position]
        getPositions _ = return []

        getPosition :: Int -> Int -> Handler Position
        getPosition _ _ = return Position

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
        getOrder _ _ _ = return Order
