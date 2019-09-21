{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API.Resolutions where

import Models
import Data.Aeson
import Servant

type ResolutionAPI =
    "resolutions" :> Get '[JSON] [Resolution] :<|>
    "resolutions" :> ReqBody '[JSON] Resolution :> Post '[JSON] Resolution :<|>
    "resolutions" :> Capture "resolutionid" Int :> Get '[JSON] (Maybe Resolution)

resolutionServer :: Server ResolutionAPI
resolutionServer = getResolutions :<|> postResolution :<|> getResolution
    where
        getResolutions :: Handler [Resolution]
        getResolutions = return []

        postResolution :: Resolution -> Handler Resolution
        postResolution = return

        getResolution :: Int -> Handler (Maybe Resolution)
        getResolution _ = return Nothing
