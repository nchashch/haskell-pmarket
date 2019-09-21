{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API.Markets where

import Models.Market
import Data.Aeson
import Servant
import GHC.Generics

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
        getMarket _ = return Market {
          marketCash = 100,
          marketB = 1
                                    }
