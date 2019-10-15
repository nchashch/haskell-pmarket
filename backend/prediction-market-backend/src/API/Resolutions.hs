{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API.Resolutions where

import Config
import Data.Aeson
import Data.Maybe (maybe)
import Database.Persist
import Models
import Servant

type ResolutionAPI =
    "resolutions" :>
    (
      Get '[JSON] [Entity Resolution] :<|>
      ReqBody '[JSON] Resolution :> Post '[JSON] ResolutionId :<|>
      Capture "resolutionid" ResolutionId :> Get '[JSON] (Entity Resolution)
    )


resolutionServer :: ServerT ResolutionAPI App
resolutionServer = getResolutions :<|> postResolution :<|> getResolution

getResolutions :: App [Entity Resolution]
getResolutions = runDb $ selectList [] []

postResolution :: Resolution -> App ResolutionId
postResolution = runDb . insert

getResolution :: ResolutionId -> App (Entity Resolution)
getResolution resolutionId = do
  resolution <- runDb $ getEntity resolutionId
  maybe (throwError err404) return resolution
