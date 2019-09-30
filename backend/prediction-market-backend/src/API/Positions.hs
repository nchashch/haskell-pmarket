{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API.Positions where

import Config
import Data.Aeson
import Data.Maybe (maybe)
import Database.Persist
import Models
import Servant

type PositionAPI =
    "positions" :> Get '[JSON] [Entity Position] :<|>
    "positions" :> Capture "positionid" PositionId :> Get '[JSON] (Entity Position) :<|>
    "markets" :> Capture "marketid" MarketId :> "positions" :> Get '[JSON] [Entity Position] :<|>
    "markets" :> Capture "marketid" MarketId :> "positions" :> Capture "outcome_index" OutcomeIndex :> Get '[JSON] (Entity Position)

positionServer :: ServerT PositionAPI App
positionServer = getPositions :<|> getPosition :<|> getMarketPositions :<|> getMarketPosition

getPositions :: App [Entity Position]
getPositions = runDb $ selectList [] []

getPosition :: PositionId -> App (Entity Position)
getPosition positionId = do
  position <- runDb $ getEntity positionId
  maybe (throwError err404) return position

getMarketPositions :: MarketId -> App [Entity Position]
getMarketPositions marketId = runDb $ selectList [PositionMarketId ==. marketId] []

getMarketPosition :: MarketId -> OutcomeIndex -> App (Entity Position)
getMarketPosition marketId outcomeIndex = do
  position <- runDb $ getBy (PrimaryPosition marketId outcomeIndex)
  maybe (throwError err404) return position
