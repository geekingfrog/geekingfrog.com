-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Data.Text
import Servant (
  Get,
  JSON,
  Proxy (..),
  (:>),
  serve,
  Server,
  ServantErr
  )

import Servant.HTML.Blaze (HTML)
import Text.Blaze (ToMarkup, toMarkup, text)
import Network.Wai (Application)
import Control.Monad.Trans.Either (EitherT)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = let port = 8080 in do
  putStrLn $ "Listening on port " ++ show port ++ "..."
  run port app

data Foo = Foo Text
instance ToMarkup Foo where
  toMarkup (Foo s) = text s

type WebsiteAPI = "foo" :> Get '[HTML] Foo

websiteApi :: Proxy WebsiteAPI
websiteApi = Proxy

websiteServer :: Server WebsiteAPI
websiteServer = return $ Foo "fooo"

app :: Application
app = serve websiteApi websiteServer
