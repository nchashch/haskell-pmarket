{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Models.Market where

import Data.Aeson
import Database.Beam

type Money = Int

data MarketT f
    = Market
    { _marketId :: C f Int
    , _cash :: C f Money
    , _b :: C f Int }
    deriving (Generic, Beamable)


type Market = MarketT Identity
deriving instance Show Market; deriving instance Eq Market
instance FromJSON Market; instance ToJSON Market

type MarketId = PrimaryKey MarketT Identity
deriving instance Show MarketId; deriving instance Eq MarketId
instance FromJSON MarketId; instance ToJSON MarketId

instance Table MarketT where
    data PrimaryKey MarketT f = MarketId (C f Int) deriving (Generic, Beamable)
    primaryKey = MarketId . _marketId
