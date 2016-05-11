{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.Views.Index where

import Data.Maybe (fromMaybe)
import Data.List (intersperse)
import Control.Applicative (liftA)
import Data.Text as T (pack, append, concat, cons, Text(..))

import Data.DateTime (toGregorian')

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Database.Persist (Entity(..))
import Geekingfrog.Db.Types as DB

import Geekingfrog.Views.Partials (
    concatTags
  , pageHead
  , navHeader
  , NavItem(..)
  , pageFooter
  )

data Index = Index [(Entity DB.Post, [Entity DB.Tag])]

instance H.ToMarkup Index where
  toMarkup (Index posts) = docTypeHtml $ do

    H.head pageHead

    body ! class_ "home" $ do
        navHeader Home
        section ! class_ "hero" $
            H.div ! class_ "container hero-container" $ h1 ! class_ "main-title main-title__huge" $ "The Geekingfrog"

            --  <div class="hero">
            --    <div class="container hero&#45;container">
            --      <h1 class="main&#45;title">Geekingfrog</h1>
            --      <div class="sub&#45;title">
            --        <div class="lines"></div>
            --        <h2>Geek stuff by a batrachian.</h2>
            --        <div class="lines"></div>
            --      </div>
            --    </div>
            --  </div>

        section ! class_ "container content" $ do
          H.div ! class_ "posts" $ do
            h2 "Blog"
            p "Some intro about my blog"
            ul $ mapM_ ((li ! class_ "post-overview") . postOverview ) posts
          H.div ! class_ "misc" $ do
            h2 "Misc stuff"
            p "Some banalities about me"
            ul $ do
              li "style items later"
              li "another item"

        pageFooter


postOverview :: (Entity Post, [Entity DB.Tag]) -> Html
postOverview (Entity postId post, tags) = a ! href (postLink post) $ do
  H.span ! class_ "date" $ text . pack $ paddedMonth ++ "/" ++ show year
  H.span ! class_ "right" $ do
    H.span ! class_ "blog-title" $ text $ postTitle post
    H.span ! class_ "blog-tags" $ text (concatTags tags)
  where
    (year, month, day) = fromMaybe (0, 0, 0) (liftA toGregorian' $ postPublishedAt post)
    paddedMonth = if month < 10 then "0" ++ show month else show month

postLink :: DB.Post -> H.AttributeValue
postLink post = H.toValue $ append "/post/" (DB.postSlug post)
