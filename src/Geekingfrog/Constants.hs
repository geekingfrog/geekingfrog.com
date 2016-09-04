module Geekingfrog.Constants where

import Data.Text
import qualified Text.Highlighting.Kate.Types as H
import qualified Text.Highlighting.Kate.Styles as H

siteUrl :: Text
siteUrl = pack "https://geekingfrog.com"

highlightStyle :: H.Style
highlightStyle = H.pygments

postsLocation :: FilePath
postsLocation = "./blog/posts/"
