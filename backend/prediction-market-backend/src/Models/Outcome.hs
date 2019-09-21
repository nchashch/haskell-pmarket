{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Models.Outcome where

import Models.Market
import Database.Persist
import Database.Persist.TH

mkPersist sqlSettings [persistLowerCase|
Outcome json
    marketId MarketId
    outstanding Int
    deriving Show
|]
