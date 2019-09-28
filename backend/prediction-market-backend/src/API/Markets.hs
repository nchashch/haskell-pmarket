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

type MarketAPI =
    "markets" :> Get '[JSON] [Entity Market] :<|>
    -- Should require authentication and authorization
    "markets" :> ReqBody '[JSON] Market :> Post '[JSON] MarketId :<|>
    "markets" :> Capture "marketid" MarketId :> Get '[JSON] (Entity Market)

marketServer :: ServerT MarketAPI App
marketServer = getMarkets :<|> postMarket :<|> getMarket

getMarkets :: App [Entity Market]
getMarkets = runDb $ selectList [] []

getMarket :: MarketId -> App (Entity Market)
getMarket id = do
  market <- runDb selectMarket
  maybe (throwError err404) return market
  where
    selectMarket :: SqlPersistT IO (Maybe (Entity Market))
    selectMarket = do
      markets <- selectList [MarketId ==. id] []
      return $ maybeHead markets

    maybeHead :: [a] -> Maybe a
    maybeHead xs = case xs of
          [] -> Nothing
          [x] -> Just x

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
