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
import Control.Monad.Reader (ReaderT, MonadReader, asks)
import Database.Persist
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH

import Config

type Money = Int
type OutcomeIndex = Int
type OutcomeAmount = Int

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Market json
    cash Money
    b Int
    numOutcomes Int
    deriving Show

Outcome json
    marketId MarketId
    outcomeIndex OutcomeIndex
    outstanding Int
    PrimaryOutcome marketId outcomeIndex
    deriving Show

Resolution json
    marketId MarketId
    outcomeIndex OutcomeIndex
    PrimaryResolution marketId outcomeIndex
    deriving Show

Order json
    marketId MarketId
    outcomeIndex OutcomeIndex
    amount OutcomeAmount
    deriving Show

Position json
    marketId MarketId
    outcomeIndex OutcomeIndex
    amount OutcomeAmount
    PrimaryPosition marketId outcomeIndex
    deriving Show
|]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks configPool
    liftIO $ runSqlPool query pool

updateBy :: (MonadIO m, PersistUniqueRead backend, PersistEntity record, PersistStoreWrite backend, PersistEntityBackend record ~ BaseBackend backend)
         => Unique record
         -> [Update record]
         -> ReaderT backend m ()
updateBy unique updates = do
  mExisting <- getBy unique
  case mExisting of
    Just (Entity key _) -> update key updates
    Nothing -> return ()
