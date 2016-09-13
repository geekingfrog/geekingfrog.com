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
        H.span "Fingerprint"
        pre "8E64 5B47 AB7A D1E0 B090  2403 42BB 864E 4735 18AA"

        H.span "Public key"
        pre $ text key

      pageFooter

key :: Text
key = "\
\ -----BEGIN PGP PUBLIC KEY BLOCK-----\n\
\ \n\
\ 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\n\
\ ZEkVGlH66Rnr/ChrqRha7Ca84Ns=\n\
\ =tRwi\n\
\ -----END PGP PUBLIC KEY BLOCK-----"
