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
\ Q5unSLL9j/rfaVXvCSGrWSpP\n\
\ =su3e\n\
\ -----END PGP PUBLIC KEY BLOCK-----\n"
