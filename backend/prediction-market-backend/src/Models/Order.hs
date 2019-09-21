{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Models.Order where

import Models.Outcome
import Database.Persist
import Database.Persist.TH

mkPersist sqlSettings [persistLowerCase|
Order json
    outcomeId OutcomeId
    amount Int
|]
