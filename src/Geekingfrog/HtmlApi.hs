{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Geekingfrog.HtmlApi where

import Control.Monad.IO.Class (liftIO)

import Servant.HTML.Blaze (HTML)
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Servant
import Data.Text (Text(..))

import qualified Geekingfrog.Views as Views
import qualified Geekingfrog.Views.Errors as Views
import qualified Geekingfrog.Queries as Queries

type HtmlAPI =
       Get '[HTML] Views.Index
  :<|> "blog" :> Get '[HTML] Views.PostsOverview
  :<|> "blog" :> "post" :> Capture "postSlug" Text :> Get '[HTML] Views.PostView
  :<|> "gpg" :> Get '[HTML] Views.GpgView


htmlHandler =
  indexHandler
  :<|> postIndexHandler
  :<|> postHandler
  :<|> gpgHandler


indexHandler :: Handler Views.Index
indexHandler = do
  postTags <- liftIO Queries.getLatestPostTags
  return $ Views.Index postTags


postIndexHandler :: Handler Views.PostsOverview
postIndexHandler = do
  postsAndTags <- liftIO Queries.getPublishedPostsAndTags
  return $ Views.PostsOverview postsAndTags


postHandler :: Text -> Handler Views.PostView
postHandler slug = do
  postAndTags <- liftIO $ Queries.getOnePostAndTags slug
  case postAndTags of
    Nothing -> throwError postNotFound
    Just p -> return $ Views.PostView p
    where postNotFound = err404 { errBody = renderMarkup errMsg}
          errMsg = Views.genericError "Not found" "No post found with this name :("

gpgHandler :: Handler Views.GpgView
gpgHandler = return Views.GpgView
