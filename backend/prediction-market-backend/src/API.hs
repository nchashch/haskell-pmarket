{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API (
    PredictionMarketAPI,
    predictionMarketServer,
    predictionMarketApp
    ) where

import API.Markets
import API.Orders
import API.Outcomes
import API.Positions
import API.Resolutions
import Servant
import Config
import Control.Monad.Reader (runReaderT)

type PredictionMarketAPI =
    ResolutionAPI :<|>
    MarketAPI :<|>
    OutcomeAPI :<|>
    PositionAPI :<|>
    OrderAPI

predictionMarketServer :: ServerT PredictionMarketAPI App
predictionMarketServer =
    resolutionServer :<|>
    marketServer :<|>
    outcomeServer :<|>
    positionServer :<|>
    orderServer

predictionMarketAPI :: Proxy PredictionMarketAPI
predictionMarketAPI = Proxy

-- | This is the function we export to run our 'UserAPI'. Given
-- a 'Config', we return a WAI 'Application' which any WAI compliant server
-- can run.
predictionMarketApp :: Config -> Application
predictionMarketApp  cfg = serve predictionMarketAPI (appToServer cfg)

-- | This function converts our @'AppT' m@ monad into the @ExceptT ServantErr
-- m@ monad that Servant's 'enter' function needs in order to run the
-- application.
convertApp :: Config -> App a -> Handler a
convertApp cfg appt = runReaderT (runApp appt) cfg

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
appToServer :: Config -> Server PredictionMarketAPI
appToServer cfg = hoistServer predictionMarketAPI (convertApp cfg) predictionMarketServer
