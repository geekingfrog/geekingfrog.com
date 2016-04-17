{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Geekingfrog.Db.Types where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Text (Text(..))

import Data.Time.Clock (UTCTime)
import Control.Applicative (liftA)

import Geekingfrog.Db.PostStatus (PostStatus)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Tag
  Id Int
  uuid Text
  name Text
  slug Text
  UniqueSlug slug
  description Text Maybe
  hidden Bool
  createdAt UTCTime default=CURRENT_TIME
  deriving Show

Post
  Id Int
  status PostStatus
  uuid Text
  slug Text
  UniquePost slug
  markdown Text
  html Text
  createdAt UTCTime default=CURRENT_TIME
  updatedAt UTCTime default=CURRENT_TIME
  language Text
  isFeatured Bool
  deriving Show

PostTag
  Id Int
  tagId TagId
  postId PostId
  UniquePostTag tagId postId
  sortOrder Int
  deriving Show

|]
