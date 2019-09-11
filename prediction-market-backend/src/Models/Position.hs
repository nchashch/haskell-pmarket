{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Models.Position where

import Models.Outcome
import Data.Aeson
import Database.Beam

type Money = Int

data PositionT f
    = Position
    { _positionId :: C f Int
    , _outcome :: PrimaryKey OutcomeT f
    , _amount :: C f Int }
    deriving (Generic, Beamable)

type Position = PositionT Identity
deriving instance Show Position; deriving instance Eq Position
instance FromJSON Position; instance ToJSON Position

type PositionId = PrimaryKey PositionT Identity
deriving instance Show PositionId; deriving instance Eq PositionId
instance FromJSON PositionId; instance ToJSON PositionId

instance Table PositionT where
    data PrimaryKey PositionT f = PositionId (C f Int) deriving (Generic, Beamable)
    primaryKey = PositionId . _positionId
