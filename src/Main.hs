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

import Index

main :: IO ()
main = let port = 8080 in do
  putStrLn $ "Listening on port " ++ show port ++ "..."
  run port app

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
