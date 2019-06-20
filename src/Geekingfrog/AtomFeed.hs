{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Geekingfrog.AtomFeed where

import Control.Applicative (liftA)
import Data.Text (Text, pack)
import qualified Data.Text.Lazy.Encoding as Tx.Enc
import Data.ByteString.Lazy.UTF8 (fromString)
import Servant hiding (Post, Link)
import Data.DateTime

import qualified Text.Blaze.Html.Renderer.String as Blaze
import Text.Atom.Feed
import qualified Text.Atom.Feed.Export as Export

import Network.HTTP.Media ((//))

import Geekingfrog.ContentType
import Geekingfrog.Urls
import Geekingfrog.Constants (siteUrl)
import qualified Geekingfrog.Types as Types

data AtomFeed = AtomFeed DateTime [Types.Post]

instance MimeRender XML AtomFeed where
  mimeRender _ atom
    = maybe (error "cannot generate feed") Tx.Enc.encodeUtf8 $ Export.textFeed $ toFeed atom

instance Accept AtomFeed where
  contentType _ = "application" // "atom+xml"

instance MimeRender AtomFeed AtomFeed where
  mimeRender _ atom
    = maybe (error "cannot generate feed") Tx.Enc.encodeUtf8 $ Export.textFeed $ toFeed atom

toFeed :: AtomFeed -> Feed
toFeed (AtomFeed genTime posts) =
  let
    feedTitle = TextString "Geekingfrog"
    feedUrl = siteUrl <> "/rss"
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
    entryId = siteUrl <> urlFor post
    entryTitle = TextString $ Types.postTitle post
    entryUpdated = toTimeRfc3339 $ simpleDateToDateTime $ Types.postCreatedAt post
    entryAuthors = [meAuthor]
    entryCategories = fmap tagToFeedCategory tags
    entryContent = Just $ HTMLContent (pack $ Blaze.renderHtml $ Types.postHtml post)
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
tagToFeedCategory tag = Category (Types.tagName tag) Nothing Nothing []

meAuthor = Person "Grégoire Charvet" (Just siteUrl) (Just "greg@geekingfrog.com") []

simpleDateToDateTime (y, m, d) = fromGregorian' y m d

toTimeRfc3339 :: DateTime -> Text
toTimeRfc3339 = pack . formatDateTime "%Y-%m-%dT%TZ"

link :: (Url a) => a -> Link
link item = Link (siteUrl <> urlFor item) Nothing Nothing Nothing Nothing Nothing [] []
