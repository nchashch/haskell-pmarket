{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API.Outcomes where

import Models.Outcome
import Models.Market
import Data.Aeson
import Servant
import GHC.Generics

type OutcomeAPI =
    "markets" :> Capture "marketid" Int :> "outcomes" :> Get '[JSON] [Outcome] :<|>
    "markets" :> Capture "marketid" Int :> "outcomes" :> Capture "outcomeid" Int :> Get '[JSON] (Maybe Outcome)

outcomeServer :: Server OutcomeAPI
outcomeServer = getOutcomes :<|> getOutcome
    where
        getOutcomes :: Int -> Handler [Outcome]
        getOutcomes _ = return []

        getOutcome :: Int -> Int -> Handler (Maybe Outcome)
        getOutcome _ _ = return Nothing
