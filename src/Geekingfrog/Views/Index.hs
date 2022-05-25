{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Geekingfrog.Views.Index where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import qualified Geekingfrog.Types as Types
import Data.Text (Text(..))

import Geekingfrog.Views.Partials (
    postOverview
  , pageHead
  , navHeader
  , NavItem(..)
  , pageFooter
  )

data Index
    = MyIndex [Types.Post]
    | CorpoIndex [Types.Post]

-- newtype Index = Index [Types.Post]

instance H.ToMarkup Index where
    toMarkup = \case
        MyIndex posts -> indexToMarkup posts
        CorpoIndex posts -> corpoIndexToMarkup posts

indexToMarkup posts = docTypeHtml $ do

      H.head $ pageHead "Geekingfrog − Geek stuff by a batrachian"

      body ! class_ "home" $ do
          navHeader (Just Home)
          section ! class_ "hero" $
              H.div ! class_ "container hero-container" $ h1 ! class_ "main-title main-title__huge" $ "The Geekingfrog"

          section ! class_ "container content" $ do
              H.div ! class_ "misc" $ do
                  h2 "About me"
                  p "I'm Greg, polyglot backend & cloud developer."
                  ul ! class_ "misc-list" $ do
                      li ! class_ "misc-item" $ b "I'm from" >> H.span " France"
                      li ! class_ "misc-item" $ b "I currently work " >> H.span " in the UK"
                      p "In 10+ years, I've built various applications for different business and scales \
                      \ (from a few views per days to millions of requests per hour)."
                      p "I've also managed and automated the deployement and monitoring of these service on AWS."
                      p "On the front end, I've translated designer vision into beautiful websites, as \
                      \ well as complex single page application in ember and react."

              H.div ! class_ "post" $ do
                  h2 "Blog"
                  p "Some brain dumps about various things, mostly coding, programming languages and open source projects. Here are the most recent posts."
                  ul ! class_ "posts-overview posts-overview__index" $
                      mapM_ ((li ! class_ "posts-overview--item") . postOverview) (take 5 posts)

          pageFooter Types.WebsitePerso


corpoIndexToMarkup posts = docTypeHtml $ do

      H.head $ pageHead "Geekin'frog LTD"

      body ! class_ "home" $ do
          navHeader (Just Home)
          section ! class_ "hero" $
              H.div ! class_ "container hero-container" $ h1 ! class_ "main-title main-title__huge" $ "Geekin'frog LTD"

          section ! class_ "container content" $ do
              H.div ! class_ "misc" $ do
                  h2 "About me"
                  p "I'm Greg, polyglot backend & cloud developer. I can help you build custom solutions in all things web, bringing experience, reliability and agile processes."
                  ul ! class_ "misc-list" $ do
                      li ! class_ "misc-item" $ b "I currently work " >> H.span " in the UK"
                      li ! class_ "misc-item" $ b "10+ years" >> H.span " of experience"
                      li ! class_ "misc-item" $ "Focused on "
                        >> b "python"
                        >> " & "
                        >> b "clojure"
                        >> " and "
                        >> b "cloud tech (aws)"
                        -- >> H.span " of experience"

                      p $ "Experienced with many storage technology like "
                        >> b "MySQL, PostgreSQL, DynamoDB"
                        >> ", "
                        >> b "event based architecture"
                        >> " (event sourcing, pub/sub with sns/sqs), and "
                        >> b "serverless"
                        >> " (aws lambda)"
                      p "I've built various applications for different business and scales \
                      \ (from a few views per days to millions of requests per hour)."
                      p "I've also managed and automated the deployement and monitoring of these service on AWS."

                      p $ "Here's "
                        >> (a ! href "/static/vrac/SeniorEngineer.pdf" $ "my resume.")

              H.div ! class_ "posts" $ do
                  h2 "Personal Blog"
                  p "Some brain dumps about various things, mostly coding, programming languages and open source projects. Here are the most recent posts."
                  ul ! class_ "posts-overview posts-overview__index" $
                      mapM_ ((li ! class_ "posts-overview--item") . postOverview) (take 5 posts)

          pageFooter Types.WebsiteCorpo
