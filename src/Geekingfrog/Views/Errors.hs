{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.Views.Errors where

import Data.Text
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Geekingfrog.Views.Partials (pageHead, navHeader, pageFooter)

testErr :: H.Markup
testErr = docTypeHtml $ do
  H.head pageHead

  body $ text "generic error here!"

notFound :: H.Markup
notFound = docTypeHtml $ do
  H.head pageHead

  body ! class_ "home" $ do
    navHeader Nothing
    section ! class_ "hero" $
      H.div ! class_ "container hero-container" $
        h1 ! class_ "main-title main-title__huge" $ "Oh noooooes!"
    section ! class_ "container content" $
      pre ! class_ "center" ! A.style "width: 50ch;" $ text frog
    pageFooter

frog :: Text
frog = "\n\
\ ---------------------------------------- \n\
  \< Couldn't find what you're looking for. >\n\
\ ---------------------------------------- \n\
\              \\   .--._.--. \n\
\               \\ ( x     x ) \n\
\                 /   . .   \\ \n\
\                 `._______.'. \n\
\               /(           )\\ \n\
\             _/  \\  \\   /  /  \\_ \n\
\           ~   `  \\  \\ /  /  '   ~. \n\
\          {    -.   \\  V  /   .-    } \n\
\        _ _`.    \\  |  |  |  /    .'_ _ \n\
\        >_       _} |  |  | {_       _< \n\
\         /. - ~ ,_-'  .^.  `-_, ~ - .\\ \n\
\                '-'|/   \\|`-` "
