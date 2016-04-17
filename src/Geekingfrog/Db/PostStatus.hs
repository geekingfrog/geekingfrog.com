{-# LANGUAGE TemplateHaskell #-}
module Geekingfrog.Db.PostStatus where

import Database.Persist.TH

data PostStatus = Published | Draft deriving (Show, Read, Enum)
derivePersistField "PostStatus"
