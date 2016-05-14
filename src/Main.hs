{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Text (Text)
import Servant hiding (Post)

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
import Database.Persist
import Database.Persist.Sqlite (runSqlite)
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))

import Geekingfrog.Types
import qualified Geekingfrog.Db.Types as DB

import Geekingfrog.Import (importData)
import Geekingfrog.Parse (parseGhostExport)

import Geekingfrog.Views.Index (Index(..))
import Geekingfrog.Views.Post (PostView(..), PostsOverview(..))
import Geekingfrog.Views.Errors (notFound, genericError)
import Geekingfrog.Queries (
    getLastPostTags
  , getPostsAndTags
  , getOnePostAndTags
  , getPostBySlug
  )


main :: IO ()
main = let port = 8080 in do
  rawContent <- B.readFile "geekingfrog.ghost.2016-04-10.json"
  let ghostExport = parseGhostExport rawContent
  case ghostExport of
    Left err -> do
      putStrLn "Parse error when importing ghost archive"
      print err
    Right (errors, (posts, tags, postTags)) -> do
      putStrLn $ "Got " ++ show (length posts) ++ " posts"
      putStrLn $ "Got " ++ show (length tags) ++ " tag"
      putStrLn $ "Got " ++ show (length postTags) ++ " posts & tags relations"
      putStrLn $ "And some errors: " ++ show errors
      importData tags posts postTags
  putStrLn "all is well"
  putStrLn $ "Listening on port " ++ show port ++ "..."
  run port app

type WebsiteAPI =
       Get '[HTML] Index
  :<|> "blog" :> Get '[HTML] PostsOverview
  :<|> "blog" :> "post" :> Capture "postSlug" Text :> Get '[HTML] PostView
  :<|> ("static" :> Raw) -- staticServer
  :<|> Raw  -- catchall for custom 404

websiteApi :: Proxy WebsiteAPI
websiteApi = Proxy

websiteServer :: Server WebsiteAPI
websiteServer = makeIndex
           :<|> makePostsIndex
           :<|> makePost
           :<|> serveDirectory "./static"
           :<|> custom404

app :: Application
app = serve websiteApi websiteServer
-- app = serve websiteApi readerServer
--   where readerServer = enter readerToHandler readerServerT

makeIndex :: Handler Index
makeIndex = do
  postTags <- liftIO getLastPostTags
  let grouped = groupPostTags postTags
  return $ Index grouped

makePostsIndex :: Handler PostsOverview
makePostsIndex = do
  let query post _ _ = do
        E.orderBy [E.asc (post ^. DB.PostPublishedAt)]
        E.where_ (E.not_ $ E.isNothing $ post ^. DB.PostPublishedAt)
  postsAndTags <- liftIO $ liftA groupPostTags (getPostsAndTags query)
  return $ PostsOverview postsAndTags


makePost :: Text -> Handler PostView
makePost slug = do
  postAndTags <- liftIO $ getOnePostAndTags slug
  case postAndTags of
    Nothing -> throwError postNotFound
    Just p -> return $ PostView p
    where postNotFound = err404 { errBody = renderMarkup errMsg}
          errMsg = genericError "Not found" "No post found with this name :("


-- assume sorted by DB.Post
groupPostTags :: [(Entity DB.Post, Entity DB.Tag)] -> [(Entity DB.Post, [Entity DB.Tag])]
groupPostTags = go []
  where go acc [] = acc
        go [] ((p, t):xs) = go [(p, [t])] xs
        go ((p, tags):rest) ((post, tag):xs) = if entityKey p == entityKey post
                                               then go ((p, tag:tags):rest) xs
                                               else go ((post, [tag]):(p, tags):rest) xs

-- Network.Wai.Request -> (Network.Wai.Response -> IO ResponseReceived) -> ResponseReceived
custom404 :: Application
custom404 _ sendResponse = sendResponse $ responseLBS H.status404
                             [("Content-Type", "text/html; charset=UTF-8")]
                             (renderMarkup notFound)
