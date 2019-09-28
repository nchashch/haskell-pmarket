{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API.Positions where

import Models
import Data.Aeson
import Servant
import Config

type PositionAPI =
    "markets" :> Capture "marketid" Int :> "positions" :> Get '[JSON] [Position] :<|>
    "markets" :> Capture "marketid" Int :> "positions" :> Capture "positionid" Int :> Get '[JSON] (Maybe Position)

positionServer :: ServerT PositionAPI App
positionServer = getPositions :<|> getPosition

getPositions :: Int -> App [Position]
getPositions _ = return []

getPosition :: Int -> Int -> App (Maybe Position)
getPosition _ _ = return Nothing
