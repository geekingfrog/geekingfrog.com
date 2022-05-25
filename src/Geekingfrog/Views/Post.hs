{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.Views.Post where

import Data.Text (unpack, pack, Text)
import Data.DateTime (toGregorian', fromGregorian', formatDateTime)
import Control.Monad (mapM_, guard)
import qualified Data.List as List

import qualified Text.Pandoc as Pandoc
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Geekingfrog.Views.Partials (
    concatTags
  , postOverview
  , pageHead
  , navHeader
  , NavItem(..)
  , pageFooter
  )

import qualified Geekingfrog.Types as Types


data PostView = PostView Types.WebsiteType Types.Post

instance H.ToMarkup PostView where
  toMarkup (PostView wt post) = docTypeHtml $ do
    H.head $ pageHead ("Geekingfrog − " <> Types.postTitle post)

    body ! class_ "blog" $ do
      navHeader (Just Blog)

      section ! class_ "hero" $
        H.div ! class_ "container hero-container" $
          h1 ! class_ "main-title main-title__blog" $ text $ Types.postTitle post

      section ! class_ "container content" $ do
        H.div ! class_ "blog-meta-container" $ H.ul $ do
          H.li ! class_ "blog-meta-section" $ do
            H.span ! class_ "name" $ "POSTED:"
            H.span ! class_ "value" $ text (formatDate $ Types.postCreatedAt post)
            -- H.span ! class_ "value" $ text (pack $ formatDate $ Types.postUpdatedAt post)
          if null $ Types.postTags post
            then text ""
            else H.li ! class_ "blog-meta-section" $ do
              H.span ! class_ "name" $ "TAGGED:"
              H.span ! class_ "value" $ text (concatTags $ Types.postTags post)

        H.div ! class_ "blog-content" $ Types.postHtml post

      pageFooter wt

data PostsOverview = PostsOverview Types.WebsiteType [Types.Post]

instance H.ToMarkup PostsOverview where
  toMarkup (PostsOverview wt posts) = docTypeHtml $ do
    H.head $ pageHead "Geekingfrog − blog"

    body ! class_ "blog" $ do
      navHeader (Just Blog)

      section $
        H.div ! class_ "hero" $ H.div ! class_ "container hero-container" $
          h1 ! class_ "main-title" $ "Blog — Geek stuff by a batrachian"  -- this an emdash u+2014

      section ! class_ "container content" $
        H.ul ! class_ "posts-overview" $
          mapM_
            ((li ! class_ "posts-overview--item posts-overview--item__blog") . postOverview)
            posts
      pageFooter wt


formatDate :: (Integer, Int, Int) -> Text
formatDate (y, m, d) = pack $ formatDateTime "%d %b %Y" $ fromGregorian' y m d
