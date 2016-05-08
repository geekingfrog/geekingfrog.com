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

import Index
import Geekingfrog.Types
import qualified Geekingfrog.Db.Types as DB
import qualified Geekingfrog.Db.PostStatus as DB

import Geekingfrog.Import (testPersistent, importData)
import Geekingfrog.Parse (parseGhostExport)

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
      :<|> "static" :> Raw -- staticServer

websiteApi :: Proxy WebsiteAPI
websiteApi = Proxy

websiteServer :: Server WebsiteAPI
websiteServer = makeIndex
           :<|> serveDirectory "./static"

app :: Application
app = serve websiteApi websiteServer
-- app = serve websiteApi readerServer
--   where readerServer = enter readerToHandler readerServerT


makeIndex = do
  postTags <- liftIO getLastPostTags
  let grouped = groupPostTags postTags
  return $ Index grouped

getLastPostTags :: IO [(Entity DB.Post, Entity DB.Tag)]
getLastPostTags = runSqlite "testing.sqlite" $ E.select $
    E.from $ \((post `E.InnerJoin` postTag) `E.InnerJoin` tag) -> do
      E.where_ $ post ^. DB.PostId `E.in_` E.subList_select $ E.from (\p -> do
        E.where_ (E.not_ $ E.isNothing $ p ^. DB.PostPublishedAt)
        E.limit 5
        E.orderBy [E.desc (p ^. DB.PostPublishedAt)]
        return $ p ^. DB.PostId
        )
      E.on $ tag ^. DB.TagId E.==. postTag ^. DB.PostTagTagId
      E.on $ post ^. DB.PostId E.==. postTag ^. DB.PostTagPostId
      E.orderBy [E.desc (post ^. DB.PostPublishedAt)]
      return (post, tag)

-- assume sorted by DB.Post
groupPostTags :: [(Entity DB.Post, Entity DB.Tag)] -> [(Entity DB.Post, [Entity DB.Tag])]
groupPostTags = go []
  where go acc [] = acc
        go [] ((p, t):xs) = go [(p, [t])] xs
        go ((p, tags):rest) ((post, tag):xs) = if entityKey p == entityKey post
                                               then go ((p, tag:tags):rest) xs
                                               else go ((post, [tag]):(p, tags):rest) xs
