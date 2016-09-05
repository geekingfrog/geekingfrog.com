{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.ContentType where

import Servant
import Data.Typeable
import Network.HTTP.Media as M ((//))

import Text.XML.Light.Types
import Text.XML.Light.Output

data XML deriving Typeable

instance Accept XML where
  contentType _ = "application" M.// "xml"
