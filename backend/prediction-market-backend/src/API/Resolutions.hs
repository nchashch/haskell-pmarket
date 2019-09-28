{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API.Resolutions where

import Models
import Data.Aeson
import Servant
import Config

type ResolutionAPI =
    "resolutions" :> Get '[JSON] [Resolution] :<|>
    "resolutions" :> ReqBody '[JSON] Resolution :> Post '[JSON] Resolution :<|>
    "resolutions" :> Capture "resolutionid" Int :> Get '[JSON] (Maybe Resolution)

resolutionServer :: ServerT ResolutionAPI App
resolutionServer = getResolutions :<|> postResolution :<|> getResolution

getResolutions :: App [Resolution]
getResolutions = return []

postResolution :: Resolution -> App Resolution
postResolution = return

getResolution :: Int -> App (Maybe Resolution)
getResolution _ = return Nothing
