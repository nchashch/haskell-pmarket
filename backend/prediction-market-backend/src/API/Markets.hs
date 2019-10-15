{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API.Markets where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Models
import Config
import Servant

-- Should require authentication and authorization
type MarketAPI =
    "markets" :>
    (
      Get '[JSON] [Entity Market] :<|>
      ReqBody '[JSON] Market :> Post '[JSON] MarketId :<|>
      Capture "marketid" MarketId :> Get '[JSON] (Entity Market)
    )

marketServer :: ServerT MarketAPI App
marketServer = getMarkets :<|> postMarket :<|> getMarket

getMarkets :: App [Entity Market]
getMarkets = runDb $ selectList [] []

getMarket :: MarketId -> App (Entity Market)
getMarket marketId = do
  market <- runDb $ getEntity marketId
  maybe (throwError err404) return market

postMarket :: Market -> App MarketId
postMarket market = runDb $ do
  marketEntity <- insertEntity market
  let marketId = entityKey marketEntity
  let market = entityVal marketEntity
  let numOutcomes = marketNumOutcomes market
  mapM_ (insertOutcome marketId) [1..numOutcomes]
  return marketId

  where
    insertOutcome :: MarketId -> Int -> SqlPersistT IO (Entity Outcome)
    insertOutcome marketId outcomeIndex =
      insertEntity $
      Outcome {
        outcomeMarketId = marketId,
        outcomeOutcomeIndex = outcomeIndex,
        outcomeOutstanding = 0
      }
