{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.Urls where

import Data.Text
import Database.Esqueleto (entityVal)

import Geekingfrog.Db.Types as DB
import Geekingfrog.Views as Views
import Geekingfrog.Types as Types

class Url a where
  urlFor :: a -> Text

instance Url Views.Index where
  urlFor _ = "/"

instance Url Views.PostsOverview where
  urlFor _ = "/blog"

instance Url Views.PostView where
  urlFor (PostView post) = urlFor post

instance Url Views.GpgView where
  urlFor _ = "/gpg"

instance Url Types.Post where
  urlFor post = append "/blog/" (Types.postSlug post)
