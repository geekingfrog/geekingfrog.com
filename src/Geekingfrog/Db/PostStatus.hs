{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Geekingfrog.Db.PostStatus where

import Data.Aeson
import GHC.Generics

import Database.Persist.TH

data PostStatus = Published | Draft deriving (Show, Read, Eq, Generic)

instance ToJSON PostStatus where

derivePersistField "PostStatus"
