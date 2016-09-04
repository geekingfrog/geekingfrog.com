module Geekingfrog.Types where

import Data.HashMap.Strict as Map

import Data.Text (Text)

import Data.ByteString.Lazy.Internal (ByteString)

type PostMap = Map.HashMap Text Post

type SimpleDate = (Integer, Int, Int) -- y, m, d
data PostStatus = Published | Draft deriving (Show, Read)

data Post = Post {
    postStatus :: PostStatus
  , postTitle :: Text
  , postSlug :: Text
  , postMarkdown :: Text
  , postCreatedAt :: SimpleDate
  , postTags :: [Tag]
  } deriving (Show)

data Tag = Tag {
    tagName :: Text
  } deriving (Show)
