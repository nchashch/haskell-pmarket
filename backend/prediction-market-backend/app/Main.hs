{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai.Handler.Warp
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (runMigration, runSqlPool)
import Control.Monad.Logger (runStderrLoggingT)
import Config (Config(..))
import Models (migrateAll)

import API

main :: IO ()
main = do
  putStrLn "Starting up prediction market backend server"
  let connectionString = "host=localhost dbname=postgres user=postgres password=postgres"
  pool <- runStderrLoggingT $ createPostgresqlPool connectionString 1
  putStrLn "Running database migrations"
  runSqlPool (runMigration migrateAll) pool
  putStrLn "Migrations were run"
  putStrLn "Server started successfully"
  let cfg = Config pool

  run 8080 $ predictionMarketApp cfg
  putStrLn "Shutting down prediction market backend server"
