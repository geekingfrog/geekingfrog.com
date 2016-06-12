{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.APIViews.Tag where

import Data.Aeson
import qualified Database.Persist as Persist  -- (Entity(..), entityIdToJSON)
import qualified Data.List as List
import qualified Data.HashMap.Strict as Map
import Data.Function (on)
import qualified Data.Vector as V

import qualified Geekingfrog.Db.Types as DB

data TagIndex = TagIndex [(Persist.Entity DB.Tag, [Persist.Entity DB.PostTag])] deriving (Show)

instance ToJSON TagIndex where
  toJSON (TagIndex tags) =
    let
      groupPostTags :: [Persist.Entity DB.PostTag] -> [Persist.Entity DB.PostTag]
      groupPostTags = List.nubBy ((==) `on` Persist.entityKey)
      postTags = concatMap snd tags
    in
      object [
          ("tags", Array . V.fromList $ fmap tagWithPostIdsToJSON tags)
        -- , ("postIds", Array . fromList $ fmap (toJSON . entityKey) postTags)
        ]


tagWithPostIdsToJSON :: (Persist.Entity DB.Tag, [Persist.Entity DB.PostTag]) -> Value
tagWithPostIdsToJSON (tag, postTags) =
  let
    (Object jsonTag) = Persist.entityIdToJSON tag
    postIds = Array . V.fromList $ fmap (toJSON . DB.postTagPostId . Persist.entityVal) postTags
  in
    Object $ Map.insert "postIds" postIds jsonTag
