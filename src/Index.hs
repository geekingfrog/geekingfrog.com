{-# LANGUAGE OverloadedStrings #-}

module Index where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

data Index = Index

instance H.ToMarkup Index where
  toMarkup _ = docTypeHtml $ do
    H.head $ do
      meta ! charset "utf-8"
      H.title "Geekingfrog"
    body ! class_ "home" $ h1 "Geek stuff by a batrachian"
