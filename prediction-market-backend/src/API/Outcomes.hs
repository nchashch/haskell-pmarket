{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module API.Outcomes where

import Models.Outcome
import Models.Market
import Data.Aeson
import Servant
import GHC.Generics

type OutcomeAPI =
    "markets" :> Capture "marketid" Int :> "outcomes" :> Get '[JSON] [Outcome] :<|>
    "markets" :> Capture "marketid" Int :> "outcomes" :> Capture "outcomeid" Int :> Get '[JSON] Outcome

outcomeServer :: Server OutcomeAPI
outcomeServer = getOutcomes :<|> getOutcome
    where
        getOutcomes :: Int -> Handler [Outcome]
        getOutcomes _ = return []

        getOutcome :: Int -> Int -> Handler Outcome
        getOutcome _ _ =
          return Outcome {
          _outcomeId = 1,
          _market = MarketId 1,
          _probability = 1,
          _outstanding = 1
                         }
