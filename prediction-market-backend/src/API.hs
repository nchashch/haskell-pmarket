{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API (
    PredictionMarketAPI,
    predictionMarketServer,
    predictionMarket
    ) where

import API.Markets
import API.Orders
import API.Outcomes
import API.Positions
import API.Resolutions
import Servant

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
