{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Text (Text)
import Servant hiding (Post)

import Servant.HTML.Blaze (HTML)
import Text.Blaze (ToMarkup, toMarkup, text)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

import qualified Data.ByteString as B (readFile)
import Data.Either (lefts, rights)
import Data.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Either
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO, MonadIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.Class
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import Safe (headMay)

import Geekingfrog.Types
import qualified Geekingfrog.Db.Types as DB
import qualified Geekingfrog.Db.PostStatus as DB

import Geekingfrog.Import (testPersistent, importData)
import Geekingfrog.Parse (parseGhostExport)

import Geekingfrog.Views.Index (Index(..))
import Geekingfrog.Views.Post (PostView(..))

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
  -- testPersistent
  putStrLn $ "Listening on port " ++ show port ++ "..."
  run port app

type WebsiteAPI =
       Get '[HTML] Index
  :<|> "blog" :> Capture "postSlug" Text :> Get '[HTML] PostView
  :<|> ("static" :> Raw) -- staticServer

websiteApi :: Proxy WebsiteAPI
websiteApi = Proxy

websiteServer :: Server WebsiteAPI
websiteServer = makeIndex
           :<|> makePost
           :<|> serveDirectory "./static"

app :: Application
app = serve websiteApi websiteServer
-- app = serve websiteApi readerServer
--   where readerServer = enter readerToHandler readerServerT

makeIndex :: Handler Index
makeIndex = do
  postTags <- liftIO getLastPostTags
  let grouped = groupPostTags postTags
  return $ Index grouped

makePost :: Text -> Handler PostView
makePost slug = do
  postAndTags <- liftIO $ getPostAndTags slug
  case postAndTags of
    Nothing -> throwError myerr
    Just p -> return $ PostView p
  where myerr :: ServantErr
        myerr = err500 { errBody = "oops" }  -- TODO better error handling

getLastPostTags :: IO [(Entity DB.Post, Entity DB.Tag)]
getLastPostTags = runSqlite "testing.sqlite" $ E.select $
    E.from $ \((post `E.InnerJoin` postTag) `E.InnerJoin` tag) -> do
      E.where_ $ post ^. DB.PostId `E.in_` subPosts
      E.on $ tag ^. DB.TagId E.==. postTag ^. DB.PostTagTagId
      E.on $ post ^. DB.PostId E.==. postTag ^. DB.PostTagPostId
      E.orderBy [E.asc (post ^. DB.PostPublishedAt)]
      return (post, tag)
    where subPosts = E.subList_select $ E.from $
            \p -> do
              E.where_ (E.not_ $ E.isNothing $ p ^. DB.PostPublishedAt)
              E.limit 5
              E.orderBy [E.desc (p ^. DB.PostPublishedAt)]
              return $ p ^. DB.PostId

-- getPostBySlug :: Text -> IO [Entity DB.Post]
-- getPostBySlug slug = runSqlite "testing.sqlite" $ E.select $ E.from $ \p -> do
--   E.where_ $ p ^. DB.PostSlug E.==. E.val slug
--   return p

getPostAndTags :: Text -> IO (Maybe (Entity DB.Post, [Entity DB.Tag]))
getPostAndTags postSlug = do
  postAndTags <- runSqlite "testing.sqlite" $ E.select $
    E.from $ \((post `E.InnerJoin` postTag) `E.InnerJoin` tag) -> do
      E.on $ tag ^. DB.TagId E.==. postTag ^. DB.PostTagTagId
      E.on $ post ^. DB.PostId E.==. postTag ^. DB.PostTagPostId
      E.where_ (post ^. DB.PostSlug E.==. E.val postSlug)
      return (post, tag)
  let tags = map snd postAndTags
  case headMay postAndTags of
    Nothing -> return Nothing
    Just (post, _) -> return $ Just (post, tags)


getPostBySlug slug = runSqlite "testing.sqlite" $ getBy $ DB.UniquePostSlug slug

-- assume sorted by DB.Post
groupPostTags :: [(Entity DB.Post, Entity DB.Tag)] -> [(Entity DB.Post, [Entity DB.Tag])]
groupPostTags = go []
  where go acc [] = acc
        go [] ((p, t):xs) = go [(p, [t])] xs
        go ((p, tags):rest) ((post, tag):xs) = if entityKey p == entityKey post
                                               then go ((p, tag:tags):rest) xs
                                               else go ((post, [tag]):(p, tags):rest) xs
