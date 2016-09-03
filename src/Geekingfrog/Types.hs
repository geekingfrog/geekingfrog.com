module Geekingfrog.Types where

import GHC.Generics (Generic)

import Data.Text (Text)

import Data.ByteString.Lazy.Internal (ByteString)

type SimpleDate = (Integer, Int, Int) -- y, m, d
data PostStatus = Published | Draft deriving (Show, Read)

data Post = Post {
    postStatus :: PostStatus
  , postTitle :: Text
  , postSlug :: Text
  , postMarkdown :: Text
  , postCreatedAt :: SimpleDate
  } deriving (Show)

data Tag = Tag {
    tagName :: Text
  } deriving (Show)
