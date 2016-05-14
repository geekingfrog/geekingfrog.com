{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.Views.Gpg where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Geekingfrog.Views.Partials (
    pageHead
  , navHeader
  , NavItem(..)
  , pageFooter
  )

data GpgView = GpgView deriving (Show)

instance H.ToMarkup GpgView where
  toMarkup GpgView = docTypeHtml $ do

    H.head pageHead

    body ! class_ "gpg" $ do
      navHeader (Just Gpg)
      section ! class_ "hero" $
        H.div ! class_ "container hero-container" $
          h1 ! class_ "main-title" $ "Gpg"

      pageFooter
