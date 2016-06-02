{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (unlines)
import Data.Text (Text, unlines, append)
import qualified Data.Text.IO as T (readFile, writeFile)
import Data.DateTime (getCurrentTime)
import Servant

import Servant.HTML.Blaze (HTML)
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)

import Network.Wai.Handler.Warp.Internal
import Network.HTTP.Types as H

import qualified Data.ByteString as B (readFile)
import Control.Applicative (liftA)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (SomeException, fromException)
import System.Directory (doesFileExist)
import Database.Persist
import Database.Persist.Sqlite (runSqlite)
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))

import Geekingfrog.Types
import qualified Geekingfrog.Db.Types as DB

import Geekingfrog.Parse (parseGhostExport)
import Geekingfrog.AtomFeed (AtomFeed(..))
import Geekingfrog.ContentType
import Geekingfrog.Constants (siteUrl)

import Geekingfrog.Views.Errors (notFound, genericError)
import qualified Geekingfrog.Urls as Urls

import Geekingfrog.Queries (
    getLastPostTags
  , getAllPostsAndTags
  , getOnePostAndTags
  , getPostBySlug
  )

import qualified Geekingfrog.Views as Views


main :: IO ()
main = let port = 8080 in do
  generateSitemap
  putStrLn $ "Listening on port " ++ show port ++ "..."
  run port app

type WebsiteAPI =
       Get '[HTML] Views.Index
  :<|> "blog" :> Get '[HTML] Views.PostsOverview
  :<|> "blog" :> "post" :> Capture "postSlug" Text :> Get '[HTML] Views.PostView
  :<|> "gpg" :> Get '[HTML] Views.GpgView
  :<|> "rss" :> Get '[XML] AtomFeed
  :<|> "robots.txt" :> Get '[PlainText] Text
  :<|> ("static" :> Raw) -- staticServer
  :<|> Raw  -- catchall for custom 404

websiteApi :: Proxy WebsiteAPI
websiteApi = Proxy

websiteServer :: Server WebsiteAPI
websiteServer = makeIndex
           :<|> makePostsIndex
           :<|> makePost
           :<|> return Views.GpgView
           :<|> makeFeed
           :<|> serveRobots
           :<|> serveDirectory "./static"
           :<|> custom404

app :: Application
app = serve websiteApi websiteServer
-- app = serve websiteApi readerServer
--   where readerServer = enter readerToHandler readerServerT

makeIndex :: Handler Views.Index
makeIndex = do
  postTags <- liftIO getLastPostTags
  return $ Views.Index postTags

makePostsIndex :: Handler Views.PostsOverview
makePostsIndex = do
  -- let query post _ _ = do
  --       E.orderBy [E.asc (post ^. DB.PostPublishedAt)]
  --       E.where_ (E.not_ $ E.isNothing $ post ^. DB.PostPublishedAt)
  -- postsAndTags <- liftIO $ liftA groupPostTags (getPostsAndTags query)
  postsAndTags <- liftIO getAllPostsAndTags
  return $ Views.PostsOverview postsAndTags


makePost :: Text -> Handler Views.PostView
makePost slug = do
  postAndTags <- liftIO $ getOnePostAndTags slug
  case postAndTags of
    Nothing -> throwError postNotFound
    Just p -> return $ Views.PostView p
    where postNotFound = err404 { errBody = renderMarkup errMsg}
          errMsg = genericError "Not found" "No post found with this name :("

makeFeed :: Handler AtomFeed
makeFeed = do
  -- let query post _ _ = do
  --       E.orderBy [E.asc (post ^. DB.PostPublishedAt)]
  --       E.where_ (E.not_ $ E.isNothing $ post ^. DB.PostPublishedAt)
  -- postsAndTags <- liftIO $ liftA groupPostTags (getPostsAndTags query)
  postsAndTags <- liftIO getAllPostsAndTags
  now <- liftIO getCurrentTime
  -- LIMIT in the query doesn't work since it intefere with the joins conditions -_-
  -- For the moment, just fetch everything and use `Data.List.take`
  return $ AtomFeed now (take 10 postsAndTags)

-- Network.Wai.Request -> (Network.Wai.Response -> IO ResponseReceived) -> ResponseReceived
custom404 :: Application
custom404 _ sendResponse = sendResponse $ responseLBS H.status404
                             [("Content-Type", "text/html; charset=UTF-8")]
                             (renderMarkup notFound)

serveRobots :: Handler Text
serveRobots = do
  exists <- liftIO (doesFileExist "./robots.txt")
  if exists
    then liftIO (T.readFile "./robots.txt")
    else throwError err404

generateSitemap :: IO ()
generateSitemap = do
  posts <- liftIO getAllPostsAndTags
  let fixedUrls = [
            -- not completely accurate, but for urlFor this doesn't matter
            Urls.urlFor $ Views.Index posts
          , Urls.urlFor Views.GpgView
          , Urls.urlFor $ Views.PostsOverview posts
          , Urls.urlFor Views.GpgView
        ]
  let postsUrls = fmap (Urls.urlFor . Views.PostView) posts
  let urls = fmap (append siteUrl) (fixedUrls ++ postsUrls)
  T.writeFile "./sitemap.txt" (unlines urls)
