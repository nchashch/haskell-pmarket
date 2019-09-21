{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Models.Resolution where

import Models.Market
import Models.Outcome
import Database.Persist
import Database.Persist.TH

mkPersist sqlSettings [persistLowerCase|
Resolution json
    marketId MarketId
    outcomeId OutcomeId
|]
