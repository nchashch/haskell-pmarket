{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Models where

import Database.Persist
import Database.Persist.TH

type Money = Int

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Market json
    cash Money
    b Int
    deriving Show

Outcome json
    marketId MarketId
    outstanding Int
    deriving Show

Resolution json
    marketId MarketId
    outcomeId OutcomeId
    deriving Show

Order json
    outcomeId OutcomeId
    amount Int
    deriving Show

Position json
    outcomeId OutcomeId
    amount Int
    deriving Show
|]
