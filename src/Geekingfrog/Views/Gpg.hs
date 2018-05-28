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
        H.p "Last updated Monday May 28 2018."
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
\ =RL7/\n\
\ -----END PGP PUBLIC KEY BLOCK-----\n"
