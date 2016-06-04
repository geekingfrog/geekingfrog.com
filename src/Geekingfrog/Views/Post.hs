{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Geekingfrog.Views.Post where

import Database.Persist (Entity(..), entityIdToJSON)
import Data.Text (unpack, pack)
import Data.DateTime (toGregorian', formatDateTime)
import Control.Monad (mapM_)
import Data.List (nubBy)

import qualified Data.HashMap.Strict as Map

import qualified Data.Vector as Vector
import Data.Aeson as Aeson
import Data.Aeson.Types as AT

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Geekingfrog.Db.Types as DB
import Geekingfrog.Views.Partials (
    concatTags
  , postOverview
  , pageHead
  , navHeader
  , NavItem(..)
  , pageFooter
  )


-- this is going to be handy in a couple of places
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


data PostView = PostView (Entity DB.Post, [Entity DB.Tag])

instance H.ToMarkup PostView where
  toMarkup (PostView (Entity postId post, tags)) = docTypeHtml $ do
    H.head pageHead

    body ! class_ "blog" $ do
      navHeader (Just Blog)

      section ! class_ "hero" $
        H.div ! class_ "container hero-container" $
          h1 ! class_ "main-title main-title__blog" $ text $ DB.postTitle post

      section ! class_ "container content" $ do
        H.div ! class_ "blog-meta-container" $ H.ul $ do
          H.li ! class_ "blog-meta-section" $ do
            H.span ! class_ "name" $ "LAST UPDATED:"
            H.span ! class_ "value" $ text (pack $ formatDateTime "%d %b %Y" $ DB.postUpdatedAt post)
          H.li ! class_ "blog-meta-section" $ do
            H.span ! class_ "name" $ "TAGGED:"
            H.span ! class_ "value" $ text (concatTags tags)

        preEscapedString (unpack $ DB.postHtml post)

      pageFooter

instance ToJSON PostView where
  toJSON (PostView (post, tags)) = postTagsToJSON (post, tags)

data PostsOverview = PostsOverview [(Entity DB.Post, [Entity DB.Tag])]

instance H.ToMarkup PostsOverview where
  toMarkup (PostsOverview posts) = docTypeHtml $ do
    H.head pageHead

    body ! class_ "blog" $ do
      navHeader (Just Blog)

      section $
        H.div ! class_ "hero" $ H.div ! class_ "container hero-container" $
          h1 ! class_ "main-title" $ "Blog — Geek stuff by a batrachian"  -- this an emdash u+2014

      section ! class_ "container content" $
        H.ul ! class_ "posts-overview" $
          mapM_ ((li ! class_ "posts-overview--item posts-overview--item__blog") . postOverview) posts
      pageFooter

instance ToJSON PostsOverview where
  toJSON (PostsOverview posts) = Aeson.object [
      ("posts", Aeson.Array . Vector.fromList $ fmap postWithTagIdsToJSON posts),
      ("tags", Aeson.Array . Vector.fromList $ fmap entityIdToJSON groupedTags)
    ]
    where
      groupedTags = groupTags $ concatMap snd posts

groupTags :: [Entity DB.Tag] -> [Entity DB.Tag]
groupTags = nubBy compareKey
  where compareKey (Entity key1 _) (Entity key2 _) = key1 == key2
