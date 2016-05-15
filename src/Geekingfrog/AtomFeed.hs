{-# LANGUAGE MultiParamTypeClasses #-}

module Geekingfrog.AtomFeed where

import Control.Applicative (liftA)
import Data.Text (unpack)
import Data.ByteString.Lazy.UTF8 (fromString)
import Servant hiding (Post)
import Database.Persist.Types (entityVal)
import Database.Esqueleto (Entity)
import Data.DateTime

import Text.XML.Light.Output (showTopElement)

import Text.Atom.Feed
import Text.Atom.Feed.Export

import Geekingfrog.Db.Types
import Geekingfrog.ContentType

data AtomFeed = AtomFeed DateTime [(Entity Post, [Entity Tag])]

instance MimeRender XML AtomFeed where
  mimeRender _ = fromString . showTopElement . xmlFeed . toFeed

toFeed :: AtomFeed -> Feed
toFeed (AtomFeed genTime posts) =
  let
    feedTitle = TextString "feed title"
    feedUrl = siteUrl ++ "/rss"
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

postToFeedEntry :: (Entity Post, [Entity Tag]) -> Entry
postToFeedEntry (postEntity, tagsEntity) =
  let
    post = entityVal postEntity
    tags = fmap entityVal tagsEntity
    entryId = siteUrl ++ "/blog/" ++ unpack (postSlug post)
    entryTitle = TextString $ unpack $ postTitle post
    entryUpdated = toTimeRfc3339 $ postUpdatedAt post
    entryAuthors = [meAuthor]
    entryCategories = fmap tagToFeedCategory tags
    entryContent = Just $ HTMLContent (unpack $ postHtml post)
    entryContributor = []
    entryLinks = []  -- TODO generate url from post
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

siteUrl = "https://geekingfrog.com"
meAuthor = Person "Grégoire Charvet" (Just siteUrl) (Just "greg@geekingfrog.com") []

toTimeRfc3339 :: DateTime -> String
toTimeRfc3339 = formatDateTime "%Y-%m-%dT%TZ"
