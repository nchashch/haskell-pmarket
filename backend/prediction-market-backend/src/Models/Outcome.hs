{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Models.Outcome where

import Models.Market
import Data.Aeson
import Database.Beam

type Money = Int

data OutcomeT f
    = Outcome
    { _outcomeId :: C f Int
    , _market :: PrimaryKey MarketT f
    , _probability :: C f Float
    , _outstanding :: C f Int }
    deriving (Generic, Beamable)

type Outcome = OutcomeT Identity
deriving instance Show Outcome; deriving instance Eq Outcome
instance FromJSON Outcome; instance ToJSON Outcome

type OutcomeId = PrimaryKey OutcomeT Identity
deriving instance Show OutcomeId; deriving instance Eq OutcomeId
instance FromJSON OutcomeId; instance ToJSON OutcomeId

instance Table OutcomeT where
    data PrimaryKey OutcomeT f = OutcomeId (C f Int) deriving (Generic, Beamable)
    primaryKey = OutcomeId . _outcomeId
