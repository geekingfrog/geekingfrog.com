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

import Text.Atom.Feed
import Text.Atom.Feed.Export

import Network.HTTP.Media ((//))

import Geekingfrog.Db.Types
import Geekingfrog.ContentType
import Geekingfrog.Urls
import Geekingfrog.Constants (siteUrl)
import qualified Geekingfrog.Types as Types

data AtomFeed = AtomFeed DateTime [Types.Post] deriving (Show)

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
    entryTitle = TextString $ unpack $ postTitle post
    entryUpdated = toTimeRfc3339 $ postUpdatedAt post
    entryAuthors = [meAuthor]
    entryCategories = fmap tagToFeedCategory tags
    entryContent = Just $ HTMLContent (unpack $ postHtml post)
    entryContributor = []
    entryLinks = [link post]
    entryPublished = liftA toTimeRfc3339 $ postPublishedAt post
    entryRights = Nothing
    entrySource = Nothing
    entrySummary = Nothing
    entryInReplyTo = Nothing
    entryInReplyTotal = Nothing
    entryAttrs = []
    entryOther = []
  in
    Entry entryId entryTitle entryUpdated entryAuthors entryCategories entryContent entryContributor entryLinks entryPublished entryRights entrySource entrySummary entryInReplyTo entryInReplyTotal entryAttrs entryOther

tagToFeedCategory :: Tag -> Category
tagToFeedCategory tag = Category (unpack $ tagName tag) Nothing Nothing []

meAuthor = Person "Grégoire Charvet" (Just $ unpack siteUrl) (Just "greg@geekingfrog.com") []

toTimeRfc3339 :: DateTime -> String
toTimeRfc3339 = formatDateTime "%Y-%m-%dT%TZ"

link :: (Url a) => a -> Link
link item = Link (unpack siteUrl ++ unpack (urlFor item)) Nothing Nothing Nothing Nothing Nothing [] []
