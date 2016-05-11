{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.Views.Errors where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Geekingfrog.Views.Partials (pageHead)

testErr :: H.Markup
testErr = docTypeHtml $ do
  H.head pageHead

  body $ text "generic error here!"
