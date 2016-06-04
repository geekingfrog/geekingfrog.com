{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.Views.Tag where

import Database.Persist (Entity(..), entityIdToJSON)

import Data.List
import Data.Function (on)
import Data.Vector (fromList)
import qualified Data.HashMap.Strict as Map
import Data.Aeson
import Geekingfrog.Db.Types as DB

data TagsOverview = TagsOverview [(Entity DB.Tag, [Entity DB.PostTag])] deriving (Show)

instance ToJSON TagsOverview where
  toJSON (TagsOverview tags) =
    let
      groupPostTags :: [Entity DB.PostTag] -> [Entity DB.PostTag]
      groupPostTags = nubBy ((==) `on` entityKey)
      postTags = concatMap snd tags
    in
      object [
          ("tags", Array . fromList $ fmap tagWithPostIdsToJSON tags)
        -- , ("postIds", Array . fromList $ fmap (toJSON . entityKey) postTags)
        ]


tagWithPostIdsToJSON :: (Entity DB.Tag, [Entity DB.PostTag]) -> Value
tagWithPostIdsToJSON (tag, postTags) =
  let
    (Object jsonTag) = entityIdToJSON tag
    postIds = Array . fromList $ fmap (toJSON . postTagPostId . entityVal) postTags
  in
    Object $ Map.insert "postIds" postIds jsonTag
