{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API.Positions where

import Models
import Data.Aeson
import Servant

type PositionAPI =
    "markets" :> Capture "marketid" Int :> "positions" :> Get '[JSON] [Position] :<|>
    "markets" :> Capture "marketid" Int :> "positions" :> Capture "positionid" Int :> Get '[JSON] (Maybe Position)

positionServer :: Server PositionAPI
positionServer = getPositions :<|> getPosition
    where
        getPositions :: Int -> Handler [Position]
        getPositions _ = return []

        getPosition :: Int -> Int -> Handler (Maybe Position)
        getPosition _ _ = return Nothing
