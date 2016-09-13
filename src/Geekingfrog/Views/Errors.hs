{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.Views.Errors where

import Data.Text as T
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Geekingfrog.Views.Partials (pageHead, navHeader, pageFooter)

genericError :: Text -> Text -> H.Markup
genericError title msg = docTypeHtml $ do
  H.head $ pageHead Nothing

  body ! class_ "home" $ do
    navHeader Nothing
    section ! class_ "hero" $
      H.div ! class_ "container hero-container" $
        h1 ! class_ "main-title main-title__huge" $ text title
    section ! class_ "container content" $
      pre ! class_ "center" ! A.style "width: 50ch;" $ text (frogWithText msg)
    pageFooter

notFound = genericError "Oh noooooes!" "Couldn't find what you were looking for."

frogWithText :: Text -> Text
frogWithText text = let
  len = T.length text + 2
  line = append " " $ append (T.replicate len "-") "\n"
  wrappedText = T.concat ["| ", text, " |\n"]
  in T.concat [line, wrappedText, line, frog]

frog :: Text
frog = "\
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
