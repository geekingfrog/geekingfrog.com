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

import Data.HashMap.Strict (fromList)
import Data.Aeson
import Data.Aeson.Types

import Data.Time.Clock (UTCTime)
import Control.Applicative (liftA)

import Geekingfrog.Db.PostStatus (PostStatus)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Tag
  Id Int
  uuid Text
  name Text
  slug Text
  UniqueTagSlug slug
  description Text Maybe
  hidden Bool
  createdAt UTCTime default=CURRENT_TIME
  deriving Show

Post
  Id Int
  status PostStatus
  uuid Text
  title Text
  slug Text
  UniquePostSlug slug
  markdown Text
  html Text
  createdAt UTCTime default=CURRENT_TIME
  updatedAt UTCTime default=CURRENT_TIME
  publishedAt UTCTime Maybe
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


instance ToJSON Tag where
  toJSON t = object [
      ("uuid", toJSON $ tagUuid t)
    , ("name", toJSON $ tagName t)
    , ("slug", toJSON $ tagSlug t)
    , ("description", toJSON $ tagDescription t)
    , ("hidden", toJSON $ tagHidden t)
    , ("createdAt", toJSON $ tagCreatedAt t)
    ]


instance ToJSON Post where
  toJSON p = Object $ fromList [
      ("status", toJSON $ postStatus p)
    , ("uuid", toJSON $ postUuid p)
    , ("title", toJSON $ postTitle p)
    , ("slug", toJSON $ postSlug p)
    , ("markdown", toJSON $ postMarkdown p)
    , ("html", toJSON $ postHtml p)
    , ("createdAt", toJSON $ postCreatedAt p)
    , ("updatedAt", toJSON $ postUpdatedAt p)
    , ("publishedAt", toJSON $ postPublishedAt p)
    , ("language", toJSON $ postLanguage p)
    , ("isFeatured", toJSON $ postIsFeatured p)
    ]

instance ToJSON PostTag where
  toJSON pt = Object $ fromList [
      ("tagId", toJSON $ postTagTagId pt)
    , ("postId", toJSON $ postTagPostId pt)
    -- no sort order, may drop that later altogether
    ]
