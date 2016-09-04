{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Geekingfrog.HtmlApi where

import Control.Monad.IO.Class (liftIO)

import qualified Data.List as List
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Servant
import Data.Text (Text(..))
import qualified Data.HashMap.Strict as Map

import qualified Geekingfrog.Views as Views
import qualified Geekingfrog.Views.Errors as Views
import qualified Geekingfrog.Queries as Queries
import Geekingfrog.Types as Types

type HtmlAPI =
       Get '[HTML] Views.Index
  :<|> "blog" :> Get '[HTML] Views.PostsOverview
  :<|> "blog" :> "post" :> Capture "postSlug" Text :> Get '[HTML] Views.PostView
  :<|> "gpg" :> Get '[HTML] Views.GpgView


htmlHandler postMap =
  indexHandler postMap
  :<|> postIndexHandler postMap
  :<|> postHandler postMap
  :<|> gpgHandler


indexHandler :: Types.PostMap -> Handler Views.Index
indexHandler postMap =
  let
    posts = Map.elems postMap
    sortedPosts = reverse $ List.sortOn Types.postCreatedAt posts
  in
    return $ Views.Index sortedPosts


postIndexHandler :: Types.PostMap -> Handler Views.PostsOverview
postIndexHandler postMap = return $ Views.PostsOverview (Map.elems postMap)


postHandler :: Types.PostMap -> Text -> Handler Views.PostView
postHandler postMap slug =
  case Map.lookup slug postMap of
    Nothing -> throwError postNotFound
    Just p -> return $ Views.PostView p
    where postNotFound = err404 { errBody = renderMarkup errMsg}
          errMsg = Views.genericError "Not found" "No post found with this name :("

gpgHandler :: Handler Views.GpgView
gpgHandler = return Views.GpgView
