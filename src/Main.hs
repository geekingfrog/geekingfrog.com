{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Text (Text)
import Servant

import Servant.HTML.Blaze (HTML)
import Text.Blaze (ToMarkup, toMarkup, text)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

import qualified Data.ByteString as B (readFile)
import Data.Either (lefts, rights)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Either
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO, MonadIO)
import Database.Persist
import Database.Persist.Sqlite

import Index
import Geekingfrog.Types
import qualified Geekingfrog.Db.Types as DB

import Geekingfrog.Import (testPersistent, importData)
import Geekingfrog.Parse (parseGhostExport)

main :: IO ()
main = let port = 8080 in do
  rawContent <- B.readFile "geekingfrog.ghost.2016-02-21.json"
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
  posts <- liftIO $ runSqlite "testing.sqlite" $ do
    -- post <- selectList [DB.PostUuid ==. "b5638535-7891-432e-9671-346811c30691"] []
    post <- selectList ([] :: [Filter DB.Post]) []
    return [post]
  return $ Index []

--------------------------------------------------------------------------------
--  Servant 0.7 doesn't have a way to have Raw inside another monad
--  So for the time being, just connect to the db on every connection
--  instead of having a pool in a reader
--------------------------------------------------------------------------------
-- readerToHandler :: Reader String :~> Handler
-- readerToHandler = Nat readerToHandler'
--
-- readerToHandler' :: forall a. Reader String a -> Handler a
-- readerToHandler' r = return (runReader r "hi")
--
-- -- Change the Reader parameter to be what I need
-- -- (something to hold db connections)
-- readerServerT :: ServerT WebsiteAPI (Reader String)
-- readerServerT = return Index :<|> serveDirectory "./static"
