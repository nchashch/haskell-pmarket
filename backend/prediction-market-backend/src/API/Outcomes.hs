{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API.Outcomes where

import Models
import Data.Aeson
import Servant
import Config
import Database.Persist
import Database.Persist.Sql (SqlPersistT)

type OutcomeAPI =
    "markets" :> Capture "marketid" MarketId :> "outcomes" :> Get '[JSON] [Entity Outcome] :<|>
    "markets" :> Capture "marketid" MarketId :> "outcomes" :> Capture "outcome_index" Int :> Get '[JSON] (Entity Outcome)

outcomeServer :: ServerT OutcomeAPI App
outcomeServer = getOutcomes :<|> getOutcome

getOutcomes :: MarketId -> App [Entity Outcome]
getOutcomes marketId = runDb $ selectList [OutcomeMarketId ==. marketId] []

getOutcome :: MarketId -> Int -> App (Entity Outcome)
getOutcome marketId outcomeIndex = do
  outcome <- runDb selectOutcome
  maybe (throwError err404) return outcome
  where
    selectOutcome :: SqlPersistT IO (Maybe (Entity Outcome))
    selectOutcome = do
      outcomes <- selectList
        [
          OutcomeMarketId ==. marketId,
          OutcomeOutcomeIndex ==. outcomeIndex
        ]
        []
      return $ maybeHead outcomes

    maybeHead :: [a] -> Maybe a
    maybeHead xs = case xs of
          [] -> Nothing
          [x] -> Just x
