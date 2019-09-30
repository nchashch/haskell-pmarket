{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API.Outcomes where

import Config
import Data.Aeson
import Database.Persist
import Models
import Servant

type OutcomeAPI =
  "markets" :> Capture "marketid" MarketId :> "outcomes" :> Get '[JSON] [Entity Outcome] :<|>
  "markets" :> Capture "marketid" MarketId :> "outcomes" :> Capture "outcome_index" OutcomeIndex :> Get '[JSON] (Entity Outcome) :<|>
  "outcomes" :> Capture "outcomeid" OutcomeId :> Get '[JSON] (Entity Outcome)

outcomeServer :: ServerT OutcomeAPI App
outcomeServer = getMarketOutcomes :<|> getMarketOutcome :<|> getOutcome

getMarketOutcomes :: MarketId -> App [Entity Outcome]
getMarketOutcomes marketId = runDb $ selectList [OutcomeMarketId ==. marketId] []

getMarketOutcome :: MarketId -> OutcomeIndex -> App (Entity Outcome)
getMarketOutcome marketId outcomeIndex = do
  outcome <- runDb $ getBy (PrimaryOutcome marketId outcomeIndex)
  maybe (throwError err404) return outcome

getOutcome :: OutcomeId -> App (Entity Outcome)
getOutcome outcomeId = do
  outcome <- runDb $ getEntity outcomeId
  maybe (throwError err404) return outcome
