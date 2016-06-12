{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Geekingfrog.JSONApi where

import Control.Monad.IO.Class (liftIO)

import Servant
import Data.Text (Text(..))

import qualified Geekingfrog.APIViews as APIViews
import qualified Geekingfrog.Queries as Queries

type JsonAPI =
       "post" :> Get '[JSON] APIViews.PostIndex
  :<|> "post" :> Capture "postSlug" Text :> Get '[JSON] APIViews.Post
  :<|> "tag" :> Get '[JSON] APIViews.TagIndex


apiHandler =
  apiPostsIndexHandler
  :<|> apiGetPostHandler
  :<|> apiTagIndexHandler


apiPostsIndexHandler :: Handler APIViews.PostIndex
apiPostsIndexHandler = do
  postsAndTags <- liftIO Queries.getAllPostsAndTags
  return $ APIViews.PostIndex postsAndTags


apiGetPostHandler :: Text -> Handler APIViews.Post
apiGetPostHandler postSlug = do
  postAndTag <- liftIO $ Queries.getOnePostAndTags postSlug
  case postAndTag of
    Nothing -> throwError err404
    Just p -> return $ APIViews.Post p


apiTagIndexHandler :: Handler APIViews.TagIndex
apiTagIndexHandler = do
  tags <- liftIO Queries.getTags
  return $ APIViews.TagIndex tags
