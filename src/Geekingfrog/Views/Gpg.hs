{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.Views.Gpg where

import Data.Text
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

    H.head $ pageHead (Just "GPG")

    body ! class_ "gpg" $ do
      navHeader (Just Gpg)
      section ! class_ "hero" $
        H.div ! class_ "container hero-container" $
          h1 ! class_ "main-title" $ "Gpg"

      section ! class_ "container content" $ do
        H.p "Last updated Friday May 1st 2020."
        H.span "Fingerprint"
        pre "74ED 801D 1315 822A 7223  4DD6 E47E 16C0 0297 7312"

        H.span "Public key"
        pre $ text key

      pageFooter


key :: Text
key
    = "\
\ -----BEGIN PGP PUBLIC KEY BLOCK-----\n\
\ \n\
\ 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\n\
\ =Jcdy\n\
\ -----END PGP PUBLIC KEY BLOCK-----\n"
