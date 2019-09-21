{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Models.Order where

import Models.Outcome
import Data.Aeson
import Database.Beam

type Money = Int

data OrderT f
    = Order
    { _orderId :: C f Int
    , _outcome :: PrimaryKey OutcomeT f
    , _amount :: C f Int }
    deriving (Generic, Beamable)

type Order = OrderT Identity
deriving instance Show Order; deriving instance Eq Order
instance FromJSON Order; instance ToJSON Order

type OrderId = PrimaryKey OrderT Identity
deriving instance Show OrderId; deriving instance Eq OrderId
instance FromJSON OrderId; instance ToJSON OrderId

instance Table OrderT where
    data PrimaryKey OrderT f = OrderId (C f Int) deriving (Generic, Beamable)
    primaryKey = OrderId . _orderId
