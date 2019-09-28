{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Models where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Database.Persist
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH

import Config

type Money = Int

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Market json
    cash Money
    b Int
    numOutcomes Int
    deriving Show

Outcome json
    marketId MarketId
    outcomeIndex Int
    outstanding Int
    PrimaryMarketOutcome marketId outcomeIndex
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


doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks configPool
    liftIO $ runSqlPool query pool
