{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Models.Position where

import Models.Outcome
import Database.Persist
import Database.Persist.TH

mkPersist sqlSettings [persistLowerCase|
Position json
    outcomeId OutcomeId
    amount Int
    deriving Show
|]
