{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Geekingfrog.AtomFeed where

import Control.Applicative (liftA)
import Data.Text (unpack)
import Data.ByteString.Lazy.UTF8 (fromString)
import Servant hiding (Post, Link)
import Database.Persist.Types (entityVal)
import Database.Esqueleto (Entity)
import Data.DateTime

import Text.XML.Light.Output (showTopElement)

import qualified Text.Blaze.Html.Renderer.String as Blaze
import Text.Atom.Feed
import Text.Atom.Feed.Export

import Network.HTTP.Media ((//))

import Geekingfrog.Db.Types
import Geekingfrog.ContentType
import Geekingfrog.Urls
import Geekingfrog.Constants (siteUrl)
import qualified Geekingfrog.Types as Types

data AtomFeed = AtomFeed DateTime [Types.Post]

instance MimeRender XML AtomFeed where
  mimeRender _ = fromString . showTopElement . xmlFeed . toFeed

instance Accept AtomFeed where
  contentType _ = "application" // "atom+xml"

instance MimeRender AtomFeed AtomFeed where
  mimeRender _ = fromString . showTopElement . xmlFeed . toFeed

toFeed :: AtomFeed -> Feed
toFeed (AtomFeed genTime posts) =
  let
    feedTitle = TextString "feed title"
    feedUrl = unpack siteUrl ++ "/rss"
    feedAuthors = [meAuthor]
    feedCategories = [Category "Programming" Nothing (Just "Programming") []]
    feedIcon = Nothing
    feedLogo = Nothing
    linkRel = Just $ Left "self"
    feedLinks = [Link feedUrl linkRel Nothing Nothing Nothing Nothing [] []]
    feedSubtitle = Just "Geek stuff by a batrachian"
    feedEntries = fmap postToFeedEntry posts
    feed = Feed feedUrl feedTitle (toTimeRfc3339 genTime) feedAuthors feedCategories [] Nothing feedIcon feedLinks Nothing Nothing Nothing feedEntries [] []
  in feed

postToFeedEntry :: Types.Post -> Entry
postToFeedEntry post =
  let
    tags = Types.postTags post
    entryId = unpack siteUrl ++ unpack (urlFor post)
    entryTitle = TextString $ unpack $ Types.postTitle post
    entryUpdated = toTimeRfc3339 $ simpleDateToDateTime $ Types.postCreatedAt post
    entryAuthors = [meAuthor]
    entryCategories = fmap tagToFeedCategory tags
    entryContent = Just $ HTMLContent (Blaze.renderHtml $ Types.postHtml post)
    entryContributor = []
    entryLinks = [link post]
    entryPublished = Just $ toTimeRfc3339 $ simpleDateToDateTime $ Types.postCreatedAt post
    entryRights = Nothing
    entrySource = Nothing
    entrySummary = Nothing
    entryInReplyTo = Nothing
    entryInReplyTotal = Nothing
    entryAttrs = []
    entryOther = []
  in
    Entry entryId entryTitle entryUpdated entryAuthors entryCategories entryContent entryContributor entryLinks entryPublished entryRights entrySource entrySummary entryInReplyTo entryInReplyTotal entryAttrs entryOther

tagToFeedCategory :: Types.Tag -> Category
tagToFeedCategory tag = Category (unpack $ Types.tagName tag) Nothing Nothing []

meAuthor = Person "Grégoire Charvet" (Just $ unpack siteUrl) (Just "greg@geekingfrog.com") []

simpleDateToDateTime (y, m, d) = fromGregorian' y m d

toTimeRfc3339 :: DateTime -> String
toTimeRfc3339 = formatDateTime "%Y-%m-%dT%TZ"

link :: (Url a) => a -> Link
link item = Link (unpack siteUrl ++ unpack (urlFor item)) Nothing Nothing Nothing Nothing Nothing [] []
