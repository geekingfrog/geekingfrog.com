{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Servant (
  Get,
  JSON,
  Proxy (..),
  (:>),
  (:<|>) (..),
  Raw,
  serve,
  serveDirectory,
  Server,
  ServantErr
  )

import Servant.HTML.Blaze (HTML)
import Text.Blaze (ToMarkup, toMarkup, text)
import Network.Wai (Application)
import Control.Monad.Trans.Either (EitherT)
import Network.Wai.Handler.Warp (run)

import qualified Data.ByteString as B (readFile)
import Data.Either (lefts, rights)

import Index
import Geekingfrog.Types

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
  -- putStrLn $ "Listening on port " ++ show port ++ "..."
  -- run port app

type WebsiteAPI =
      Get '[HTML] Index
  :<|> "static" :> Raw -- staticServer

websiteApi :: Proxy WebsiteAPI
websiteApi = Proxy

websiteServer :: Server WebsiteAPI
websiteServer = return Index
           :<|> serveDirectory "./static"

app :: Application
app = serve websiteApi websiteServer
