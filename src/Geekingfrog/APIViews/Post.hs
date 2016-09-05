{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Geekingfrog.APIViews.Post where

import Database.Persist (Entity(..), entityIdToJSON)
import Data.Aeson as Aeson
import Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as Vector
import Data.List as List


import qualified Geekingfrog.Db.Types as DB

data PostIndex = PostIndex [(Entity DB.Post, [Entity DB.Tag])]

data Post = Post (Entity DB.Post, [Entity DB.Tag])

instance ToJSON PostIndex where
  toJSON (PostIndex posts) = Aeson.object [
      ("posts", Aeson.Array . Vector.fromList $ fmap postWithTagIdsToJSON posts),
      ("tags", Aeson.Array . Vector.fromList $ fmap entityIdToJSON groupedTags)
    ]
    where
      groupedTags = groupTags $ concatMap snd posts

instance ToJSON Post where
  toJSON (Post (post, tags)) = postTagsToJSON (post, tags)


groupTags :: [Entity DB.Tag] -> [Entity DB.Tag]
groupTags = List.nubBy compareKey
  where compareKey (Entity key1 _) (Entity key2 _) = key1 == key2


postTagsToJSON :: (Entity DB.Post, [Entity DB.Tag]) -> Value
postTagsToJSON (post, tags) = Object [
    ("post", postWithTagIdsToJSON (post, tags)),
    ("tags", Aeson.Array . Vector.fromList $ fmap entityKeyToJSON tags)
  ]


entityKeyToJSON (Entity key _) = toJSON key


postWithTagIdsToJSON :: (Entity DB.Post, [Entity DB.Tag]) -> Value
postWithTagIdsToJSON (post, tags) =
  let
    (Object jsonPost) = entityIdToJSON post
    jsonTags = Aeson.Array . Vector.fromList $ fmap entityIdToJSON tags
    tagIds = Aeson.Array . Vector.fromList $ fmap entityKeyToJSON tags
  in
    Object $ Map.insert "tagIds" tagIds jsonPost
