{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Geekingfrog.Types where

import GHC.Generics (Generic)

import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile)
import Data.Aeson as A
import Data.Aeson.Types
import Control.Applicative (empty, liftA)
import Data.Text (Text (..))
import Data.Time.Clock (UTCTime)
import Data.DateTime (fromSeconds)
import Control.Monad (liftM)
import Data.Char (isUpper, toLower)

import Data.ByteString.Lazy.Internal (ByteString)

-- A couple of fields are useless but kept for compat with ghost schemas
data Post = Post {
    postId :: Int
  , postStatus :: PostStatus
  , postUuid :: Text
  , postTitle :: Text
  , postSlug :: Text
  , postMarkdown :: Text
  , postCreatedAt :: UTCTime
  , postUpdatedAt :: UTCTime
  , postPublishedAt :: Maybe UTCTime
  , postLanguage :: Text
  , postHtml :: Text
  , postImage :: Maybe Text
  , postIsFeatured :: Bool
  , postPageId :: Int
  , postMetaTitle :: Maybe Text
  , postMetaDescription :: Maybe Text
  } deriving (Show)

instance FromJSON Post where
  parseJSON (Object v) = Post
                     <$> v .: "id"
                     <*> v .: "status"
                     <*> v .: "uuid"
                     <*> v .: "title"
                     <*> v .: "slug"
                     <*> v .: "markdown"
                     <*> liftA fromMiliseconds (v .: "created_at")
                     <*> liftA fromMiliseconds (v .: "updated_at")
                     <*> (liftA . liftA) fromMiliseconds (v .: "published_at")
                     <*> v .: "language"
                     <*> v .: "html"
                     <*> v .: "image"
                     <*> liftA boolFromInt (v .: "featured")
                     <*> v .: "page"
                     <*> v .: "meta_title"
                     <*> v .: "meta_description"

  parseJSON _ = empty

fromMiliseconds :: Integer -> UTCTime
fromMiliseconds ms = fromSeconds (ms `div` 1000)

boolFromInt :: Int -> Bool
boolFromInt = (/= 0)

data PostStatus = Published | Draft deriving (Show, Read)

instance FromJSON PostStatus where
  parseJSON (A.String "published") = pure Published
  parseJSON (A.String "draft") = pure Draft
  parseJSON _ = empty

data Tag = Tag {
    tagId :: Int
  , tagUuid :: Text
  , tagName :: Text
  , tagSlug :: Text
  , tagDescription :: Maybe Text
  , tagImage :: Maybe Text
  , tagHidden :: Bool
  , tagParentId :: Maybe Int
  , tagMetaTitle :: Maybe Text
  , tagMetaDescription :: Maybe Text
  , tagCreatedAt :: UTCTime
  } deriving (Show)

instance FromJSON Tag where
  parseJSON (A.Object v) = Tag
                       <$> v .: "id"
                       <*> v .: "uuid"
                       <*> v .: "name"
                       <*> v .: "slug"
                       <*> v .: "description"
                       <*> v .: "image"
                       <*> liftA boolFromInt (v .: "hidden")
                       <*> v .: "parent_id"
                       <*> v .: "meta_title"
                       <*> v .: "meta_description"
                       <*> liftA fromMiliseconds (v .: "created_at")

  parseJSON _ = empty

data PostTag = PostTag {
    postTagId :: Int
  , postTagPostId :: Int
  , postTagTagId :: Int
  , postTagSortOrder :: Int
  } deriving (Show, Generic)

instance FromJSON PostTag where
  parseJSON = A.genericParseJSON defaultOptions {
    fieldLabelModifier = camelize . drop 7 -- tag_id -> postTagId
    }

-- super inneficient but I don't care about perf here
camelize :: String -> String
camelize field = go field ""
  where go [] acc = drop 1 $ reverse acc
        go (x:xs) acc
          | isUpper x = go xs (toLower x : '_' : acc)
          | otherwise = go xs (x:acc)
