{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.Views.Index where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Database.Persist (Entity(..))
import Geekingfrog.Db.Types as DB

import Geekingfrog.Views.Partials (
    postOverview
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
        navHeader (Just Home)
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
            p "Some of my brain dump about various things, mostly coding, programming languages and open source projects"
            ul ! class_ "posts-overview posts-overview__index" $
              mapM_ ((li ! class_ "posts-overview--item") . postOverview ) posts
          H.div ! class_ "misc" $ do
            h2 "About me"
            p "I'm Greg, full stack software engineer."
            ul ! class_ "misc-list" $ do
              li ! class_ "misc-item" $ b "I'm from" >> H.span " France"
              li ! class_ "misc-item" $ b "I currently work " >> H.span " in the UK"
            p "I've built a tile server for Opensignal as well as various REST API in the past years."
            p "I've also managed and automated the deployment and monitoring of these services on AWS."
            p "On the front end, I've translated designer vision into beautiful websites, as \
            \ well as complex single page application in ember and react."
            p "I'm interested in complex challenges to solve especially those arising from \
            \ scalability issues (amount of data or size of the code base)."

        pageFooter
