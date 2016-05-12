{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.Views.Post where

import Database.Persist (Entity(..))
import Data.Text (unpack, pack)
import Data.Maybe (fromMaybe)
import Data.DateTime (toGregorian', formatDateTime)
import Control.Applicative (liftA)

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Geekingfrog.Db.Types as DB
import Geekingfrog.Views.Partials (
    concatTags
  , pageHead
  , navHeader
  , NavItem(..)
  , pageFooter
  )


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

data PostsOverview = PostsOverview [Entity DB.Post]

instance H.ToMarkup PostsOverview where
  toMarkup (PostsOverview posts) = docTypeHtml $ do
    H.head pageHead

    body ! class_ "blog" $ do
      navHeader (Just Blog)

      section $ do
        H.div ! class_ "hero" $ H.div ! class_ "container hero-container" $ h1 ! class_ "main-title" $ "Blog"

        text "moar stuff here"
      pageFooter
