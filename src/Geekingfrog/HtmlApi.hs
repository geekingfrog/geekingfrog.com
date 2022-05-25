{-# LANGUAGE LambdaCase #-}
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
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import Data.Ord (Down(..))

import qualified Geekingfrog.Views as Views
import qualified Geekingfrog.Views.Errors as Views
import Geekingfrog.Types as Types

type HtmlAPI =
       Header "Host" Text :> Get '[HTML] Views.Index
  :<|> "blog" :> Header "Host" Text :> Get '[HTML] Views.PostsOverview
  :<|> "blog"
      :> "post"
      :> Header "Host" Text
      :> Capture "postSlug" Text
      :> Get '[HTML] Views.PostView
  :<|> "gpg" :> Header "Host" Text :> Get '[HTML] Views.GpgView


htmlHandler postMap =
  indexHandler postMap
  :<|> postIndexHandler postMap
  :<|> postHandler postMap
  :<|> gpgHandler


indexHandler :: Types.PostMap -> Maybe Text -> Handler Views.Index
indexHandler postMap hostHeader =
  let
    posts = reverse $ filter ((==) Types.Published . Types.postStatus) $ Map.elems postMap
    sortedPosts = List.sortOn (Down . Types.postCreatedAt) posts
    view = case websiteType hostHeader of
             Types.WebsiteCorpo -> Views.CorpoIndex
             Types.WebsitePerso -> Views.MyIndex
  in
    return $ view sortedPosts


postIndexHandler :: Types.PostMap -> Maybe Text -> Handler Views.PostsOverview
postIndexHandler postMap hostHeader =
  let
    posts = List.sortOn (Down . Types.postCreatedAt)
      $ filter ((== Types.Published) . Types.postStatus)
      $ Map.elems postMap
  in
    return $ Views.PostsOverview (websiteType hostHeader) posts


postHandler :: Types.PostMap -> Maybe Text -> Text -> Handler Views.PostView
postHandler postMap hostHeader slug =
  case Map.lookup slug postMap of
    Nothing -> throwError postNotFound
    Just p -> return $ Views.PostView wt p
    where postNotFound = err404 { errBody = renderMarkup errMsg}
          errMsg = Views.genericError wt "Not found" "No post found with this name :("
          wt = websiteType hostHeader

gpgHandler :: Maybe Text -> Handler Views.GpgView
gpgHandler hostHeader = return $ Views.GpgView (websiteType hostHeader)


websiteType :: Maybe Text -> Types.WebsiteType
websiteType = \case
   Just h -> if "geekinfrog.com" `T.isPrefixOf` h
                then Types.WebsiteCorpo
                else Types.WebsitePerso
   _ -> Types.WebsitePerso
