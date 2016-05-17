{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.Urls where

import Data.Text
import Database.Esqueleto (entityVal)

import Geekingfrog.Db.Types as DB
import Geekingfrog.Views as Views

class Url a where
  urlFor :: a -> Text

instance Url Views.Index where
  urlFor _ = "/"

instance Url Views.PostsOverview where
  urlFor _ = "/blog"

instance Url Views.PostView where
  urlFor (PostView (post, _)) = urlFor (entityVal post)

instance Url Views.GpgView where
  urlFor _ = "/gpg"

instance Url DB.Post where
  urlFor post = append "/blog/" (DB.postSlug post)
