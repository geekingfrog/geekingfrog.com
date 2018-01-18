module Geekingfrog.Constants where

import Data.Text
import qualified Skylighting.Types as H
import qualified Skylighting.Styles as H

siteUrl :: Text
siteUrl = pack "https://geekingfrog.com"

highlightStyle :: H.Style
highlightStyle = H.pygments

postsLocation :: FilePath
postsLocation = "./blog/posts/"
