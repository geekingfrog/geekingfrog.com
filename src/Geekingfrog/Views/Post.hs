{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.Views.Post where

import Database.Persist (Entity(..), entityIdToJSON)
import Data.Text (unpack, pack)
import Data.DateTime (toGregorian', formatDateTime)
import Control.Monad (mapM_)

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Geekingfrog.Db.PostStatus (PostStatus(..))
import Geekingfrog.Db.Types as DB
import Geekingfrog.Views.Partials (
    concatTags
  , postOverview
  , pageHead
  , navHeader
  , NavItem(..)
  , pageFooter
  )

import qualified Geekingfrog.Types as Types


data PostView = PostView Types.Post

instance H.ToMarkup PostView where
  toMarkup (PostView post) = docTypeHtml $ do
    H.head pageHead

    body ! class_ "blog" $ do
      navHeader (Just Blog)

      section ! class_ "hero" $
        H.div ! class_ "container hero-container" $
          h1 ! class_ "main-title main-title__blog" $ text $ Types.postTitle post

      section ! class_ "container content" $ do
        H.div ! class_ "blog-meta-container" $ H.ul $
          -- H.li ! class_ "blog-meta-section" $ do
          --   H.span ! class_ "name" $ "LAST UPDATED:"
          --   H.span ! class_ "value" $ text (pack $ formatDateTime "%d %b %Y" $ DB.postUpdatedAt post)
          H.li ! class_ "blog-meta-section" $ do
            H.span ! class_ "name" $ "TAGGED:"
            H.span ! class_ "value" $ text (concatTags $ Types.postTags post)

        H.div ! class_ "blog-content" $ preEscapedString (unpack $ Types.postMarkdown post)

      pageFooter

data PostsOverview = PostsOverview [Types.Post]

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
          mapM_
            ((li ! class_ "posts-overview--item posts-overview--item__blog") . postOverview)
            posts
      pageFooter
