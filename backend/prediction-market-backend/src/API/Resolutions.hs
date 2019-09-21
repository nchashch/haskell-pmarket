{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API.Resolutions where

import Models.Resolution
import Models.Market
import Models.Outcome
import Data.Aeson
import Servant
import GHC.Generics

type ResolutionAPI =
    "resolutions" :> Get '[JSON] [Resolution] :<|>
    "resolutions" :> ReqBody '[JSON] Resolution :> Post '[JSON] Resolution :<|>
    "resolutions" :> Capture "resolutionid" Int :> Get '[JSON] Resolution

resolutionServer :: Server ResolutionAPI
resolutionServer = getResolutions :<|> postResolution :<|> getResolution
    where
        getResolutions :: Handler [Resolution]
        getResolutions = return []

        postResolution :: Resolution -> Handler Resolution
        postResolution = return

        getResolution :: Int -> Handler Resolution
        getResolution _ = return $ Resolution {
          _resolutionId = 1,
          _resolvedMarket = MarketId 1,
          _actualOutcome = OutcomeId 1
                                              }
