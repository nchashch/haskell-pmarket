{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module API.Positions where

import Models.Position
import Models.Outcome
import Data.Aeson
import Servant
import GHC.Generics

type PositionAPI =
    "markets" :> Capture "marketid" Int :> "positions" :> Get '[JSON] [Position] :<|>
    "markets" :> Capture "marketid" Int :> "positions" :> Capture "positionid" Int :> Get '[JSON] Position

positionServer :: Server PositionAPI
positionServer = getPositions :<|> getPosition
    where
        getPositions :: Int -> Handler [Position]
        getPositions _ = return []

        getPosition :: Int -> Int -> Handler Position
        getPosition _ _ = return Position {
          _positionId = 1,
          _outcome = OutcomeId 1,
          _amount = 100
                                          }
