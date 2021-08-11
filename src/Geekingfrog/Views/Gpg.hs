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

    body ! class_ "gpg" $ H.div ! class_ "gpg-content" $ do
      navHeader (Just Gpg)
      section ! class_ "hero" $
        H.div ! class_ "container hero-container" $
          h1 ! class_ "main-title" $ "Gpg"

      section ! class_ "container content" $ do
        H.p "Last updated: July 25th 2021"
        H.p $ (a ! href "https://latacora.micro.blog/2019/07/16/the-pgp-problem.html" $ "This article") <> text " and the fact that I never use gpg conviced me it's not worth keeping a gpg key."
        H.p $ "If you really want to send me something encrypted, I recommend using " <> (a ! href "https://github.com/FiloSottile/age" $ "age") <> ", with the following ssh public key:"
        pre "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJG+Ud7QuiO+AT6hAnPPhJTpGVI9i833/hYAgN4fXL4A"

      pageFooter
