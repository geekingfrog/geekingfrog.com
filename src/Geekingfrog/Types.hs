module Geekingfrog.Types where

import Data.HashMap.Strict as Map

import Data.Text (Text)
import Text.Blaze.Html (Html)
import Data.ByteString.Lazy.Internal (ByteString)

type PostMap = Map.HashMap Text Post

type SimpleDate = (Integer, Int, Int) -- y, m, d
data PostStatus = Published | Draft deriving (Show, Read, Eq)

data Post = Post {
    postStatus :: PostStatus
  , postTitle :: Text
  , postSlug :: Text
  , postMarkdown :: Text
  , postHtml :: Html
  , postCreatedAt :: SimpleDate
  , postTags :: [Tag]
  }

data Tag = Tag {
    tagName :: Text
  } deriving (Show)
