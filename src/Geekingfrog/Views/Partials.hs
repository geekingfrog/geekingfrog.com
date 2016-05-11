{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.Views.Partials where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Data.Text as T (pack, append, concat, cons, Text(..))
import Data.List (intersperse)
import Database.Esqueleto

import Geekingfrog.Db.Types as DB

import Svglogo (svglogo)

concatTags :: [Entity DB.Tag] -> Text
concatTags tags = T.concat $ intersperse ", " $ fmap (cons '#' . tagSlug . entityVal) tags

-- Later, make that more robust to link that with the route. Atm, a sum type is fine
data NavItem = Home | Blog | Gpg deriving (Show, Eq)

navHeader :: NavItem -> Markup
navHeader activeItem = header $ H.div ! class_ "container header-container" $ do
  H.div ! class_ "logo" $ svglogo
  nav $ ul ! class_ "nav-links" $ do
    li ! class_ (makeClass Home activeItem) $ a ! href "/" $ "HOME"
    li ! class_ (makeClass Blog activeItem) $ a ! href "/blog" $ "BLOG"
    li ! class_ (makeClass Gpg activeItem) $ a ! href "/gpg" $ "GPG"
  where makeClass item target | item == target = "nav-link nav-link__active"
                              | otherwise = "nav-link"

pageFooter = do
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

  H.script ! type_ "text/javascript" ! src "/static/prism.js" $ mempty


pageHead = do
    meta ! charset "utf-8"
    H.title "Geekingfrog"
    link ! rel "stylesheet" ! href "/static/styles.css" ! type_ "text/css"
    link ! rel "stylesheet" ! href "/static/prism.css" ! type_ "text/css"
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
