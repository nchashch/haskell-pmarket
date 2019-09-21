{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Models.Resolution where

import Models.Market
import Models.Outcome
import Data.Aeson
import Database.Beam

data ResolutionT f
    = Resolution
    { _resolutionId :: C f Int
    , _resolvedMarket :: PrimaryKey MarketT f
    , _actualOutcome :: PrimaryKey OutcomeT f}
    deriving (Generic, Beamable)


type Resolution = ResolutionT Identity
deriving instance Show Resolution; deriving instance Eq Resolution
instance FromJSON Resolution; instance ToJSON Resolution

type ResolutionId = PrimaryKey ResolutionT Identity
deriving instance Show ResolutionId; deriving instance Eq ResolutionId
instance FromJSON ResolutionId; instance ToJSON ResolutionId

instance Table ResolutionT where
    data PrimaryKey ResolutionT f = ResolutionId (C f Int) deriving (Generic, Beamable)
    primaryKey = ResolutionId . _resolutionId
