{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Geekingfrog.Types where

import GHC.Generics (Generic)

import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile)
import Data.Aeson as A
-- import Data.Aeson.Types (defaultOptions, fieldLabelModifier)
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
  , postStatus :: Status
  , postUuid :: Text
  , postTitle :: Text
  , postSlug :: Text
  , postMarkdown :: Text
  , postAuthorId :: Int  -- for the time being
  , postCreatedAt :: UTCTime
  , postCreatedBy :: Int
  , postUpdatedAt :: UTCTime
  , postUpdatedBy :: Int
  , postPublishedAt :: Maybe UTCTime
  , postPublishedBy :: Maybe Int
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
                     <*> v .: "author_id"
                     <*> liftA fromMiliseconds (v .: "created_at")
                     <*> v .: "created_by"
                     <*> liftA fromMiliseconds (v .: "updated_at")
                     <*> v .: "updated_by"
                     <*> (liftA . liftA) fromMiliseconds (v .: "published_at")
                     <*> v .: "published_by"
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
boolFromInt = (== 0)

data Status = Published | Draft deriving (Show, Read)

instance FromJSON Status where
  parseJSON (A.String "published") = pure Published
  parseJSON (A.String "draft") = pure Draft
  parseJSON _ = empty

data User = User {
    userId :: Int
  , userUuid :: Text
  , userName :: Text
  , userSlug :: Text
  , userPassword :: Text
  , userEmail :: Text
  , userImage :: Maybe Text
  , userCover :: Maybe Text
  , userBio :: Maybe Text
  , userWebsite :: Maybe Text
  , userLocation :: Maybe Text
  , userAccessibility :: Maybe Text
  , userStatus :: UserStatus
  , userLanguage :: Text
  , userMetaTitle :: Maybe Text
  , userMetaDescription :: Maybe Text
  , userLastLogin :: Maybe UTCTime
  , userCreatedAt :: UTCTime
  , userCreatedBy :: Int
  , userUpdatedAt :: UTCTime
  , userUpdatedBy :: Int
  , tour :: Maybe Text -- no idea what's that
  } deriving (Show)

instance FromJSON User where
  parseJSON (A.Object v) = User
                       <$> v .: "id"
                       <*> v .: "uuid"
                       <*> v .: "name"
                       <*> v .: "name"
                       <*> v .: "password"
                       <*> v .: "email"
                       <*> v .: "image"
                       <*> v .: "cover"
                       <*> v .: "bio"
                       <*> v .: "website"
                       <*> v .: "location"
                       <*> v .: "accessibility"
                       <*> v .: "status"
                       <*> v .: "language"
                       <*> v .: "meta_title"
                       <*> v .: "meta_description"
                       <*> (liftA . liftA) fromMiliseconds (v .: "last_login")
                       <*> liftA fromMiliseconds (v .: "created_at")
                       <*> v .: "created_by"
                       <*> liftA fromMiliseconds (v .: "updated_at")
                       <*> v .: "updated_by"
                       <*> v .: "tour"
  parseJSON _ = empty

data UserStatus = Active | Other deriving (Show) -- no idea what to put here :/

instance FromJSON UserStatus where
  parseJSON (A.String "active") = pure Active
  parseJSON _ = pure Other

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
  , tagCreatedBy :: Int
  , tagUpdatedAt :: UTCTime
  , tagUpdatedBy :: Int
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
                       <*> v .: "created_by"
                       <*> liftA fromMiliseconds (v .: "updated_at")
                       <*> v .: "updated_by"

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
