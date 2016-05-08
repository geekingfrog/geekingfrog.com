{-# LANGUAGE OverloadedStrings #-}

module Index where

import Data.Maybe (fromMaybe)
import Data.List (intersperse)
import Control.Applicative (liftA)
import Data.Text as T (pack, concat, cons)

import Data.DateTime (toGregorian')

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Svglogo (svglogo)

import Database.Persist (Entity(..))
import Geekingfrog.Db.Types as DB

data Index = Index [(Entity DB.Post, [Entity DB.Tag])]

instance H.ToMarkup Index where
  toMarkup (Index posts) = docTypeHtml $ do

    H.head $ do
        meta ! charset "utf-8"
        H.title "Geekingfrog"
        link ! rel "stylesheet" ! href "/static/styles.css" ! type_ "text/css"
        H.style ! type_ "text/css" $
          "@font-face{ \
          \ font-family: 'Fira Sans';\
\   src: url('/static/font/FiraSans-Regular.eot');\
\   src: local('Fira Sans Regular'),\
\     url('/static/font/FiraSans-Regular.eot') format('embedded-opentype'),\
\     url('/static/font/FiraSans-Regular.woff') format('woff'),\
\     url('/static/font/FiraSans-Regular.ttf') format('truetype');\
\   font-weight: 400;\
\   font-style: normal;\
\ }\
\ \
\ @font-face{\
\     font-family: 'Fira Sans';\
\     src: url('/static/font/FiraSans-Bold.eot');\
\     src: local('Fira Sans Bold'),\
\          url('/static/font/FiraSans-Bold.eot') format('embedded-opentype'),\
\          url('/static/font/FiraSans-Bold.woff') format('woff'),\
\          url('/static/font/FiraSans-Bold.ttf') format('truetype');\
\     font-weight: 600;\
\     font-style: normal;\
\ }"

    body ! class_ "home" $ do
        header $ H.div ! class_ "container header-container" $ do
            H.div ! class_ "logo" $ svglogo
            nav $ ul ! class_ "nav-links" $ do
                li ! class_ "nav-link nav-link__active" $ a ! href "home" $ "HOME"
                li ! class_ "nav-link" $ a ! href "blog" $ "BLOG"
                li ! class_ "nav-link" $ a ! href "gpg" $ "GPG"
        section $ do
            H.div ! class_ "hero" $ H.div ! class_ "container hero-container" $ h1 ! class_ "main-title" $ "Blog"

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

            H.div ! class_ "container content" $ do
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

        footer $ H.div ! class_ "container" $ do
            H.div ! class_ "panel panel-bio" $ do
                h2 "HELLO!"
                p $ do
                    "My name is Greg. I work at "
                    a ! href "opensignal.com" $ "Opensignal"
                    "on the web and backend team."
                p "I build APIs to collect our crowdsourced data, implement designs and build the corresponding backends."
            H.div ! class_ "panel" $ do
                h2 "GET IN TOUCH"
                p $ do
                    "On "
                    a ! href "https://www.linkedin.com/in/gr%C3%A9goire-charvet-b62440aa" $ "linkedin"
                    "or at greg＠geekingfrog․com "
                    a ! href "http://lea.verou.me/2009/11/yet-another-email-hiding-technique/" $ "(don't copy paste this email."
                    ")"
            H.div ! class_ "panel" $ do
                h2 "SUBSCRIBE"
                p "RSS feed coming soon!"


postOverview :: (Entity Post, [Entity DB.Tag]) -> Html
postOverview (Entity postId post, tags) = a ! href "link" $ do
  H.span ! class_ "date" $ text . pack $ paddedMonth ++ "/" ++ show year
  H.span ! class_ "right" $ do
    H.span ! class_ "blog-title" $ text $ postTitle post
    H.span ! class_ "blog-tags" $ textTag
  where
    (year, month, day) = fromMaybe (0, 0, 0) (liftA toGregorian' $ postPublishedAt post)
    paddedMonth = if month < 10 then "0" ++ show month else show month
    textTag =  text . T.concat $ intersperse ", " $ fmap (cons '#' . DB.tagSlug . entityVal) tags
