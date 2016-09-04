{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.Parse (parseGhostExport) where

import Prelude hiding (readFile, head)
import Data.ByteString (readFile, ByteString)
import Data.Aeson (eitherDecodeStrict, Value, parseJSON, FromJSON)
import Data.Aeson.Types (
    parseEither
  , withObject
  , withArray
  , Parser
  , (.:)
  , Value(..)
  , Object
  )

import Data.Text (Text)
import Data.Vector (head)
import Control.Monad (join)
import Data.Either (lefts, rights)
import Control.Applicative (liftA, liftA3)

import Geekingfrog.GhostTypes (Post, Tag, PostTag)

parseGhostExport :: ByteString -> Either String ([String], ([Post], [Tag], [PostTag]))
parseGhostExport rawContent = do
  let eitherData = eitherDecodeStrict rawContent >>= parseEither parseData

  -- then parse the various required objects
  let eitherPosts = eitherData >>= parseEither verboseParseManyPosts
  let eitherTags = eitherData >>= parseEither verboseParseManyTags
  let eitherTags = eitherData >>= parseEither verboseParseManyTags
  let eitherPostTags = eitherData >>= parseEither verboseParseManyPostTags

  let grouped = liftA3 (,,) eitherPosts eitherTags eitherPostTags

  case grouped of
    Left err -> Left err
    Right (posts, tags, postTags) -> do
      let errors = lefts posts ++ lefts tags ++ lefts postTags
      Right (errors, (rights posts, rights tags, rights postTags))

verboseParseManyPosts :: Object -> Parser [Either String Post]
verboseParseManyPosts = verboseParseKey "posts"

verboseParseManyTags :: Object -> Parser [Either String Tag]
verboseParseManyTags = verboseParseKey "tags"

verboseParseManyPostTags :: Object -> Parser [Either String PostTag]
verboseParseManyPostTags = verboseParseKey "posts_tags"

verboseParseKey :: (FromJSON a) => Text -> Object -> Parser [Either String a]
verboseParseKey key o = do
  raw <- o .: key
  return $ fmap (join . parseEither verboseParser) raw

parseData :: Value -> Parser Object
parseData val = do
  db <- withObject "ghost export" (.: "db") val
  -- get the first (and only?) export in the `db` array
  arrDb <- withArray "db" (return . head) db
  -- Extract the data from the db
  actualData <- withObject "data" (.: "data") arrDb
  -- Make sure it's an object
  withObject "content" return actualData

verboseParser :: (FromJSON a) => Value -> Parser (Either String a)
verboseParser v = do
  let parsed = parseEither parseJSON v
  case parsed of
    Left err -> return . Left $ err ++ " -- Invalid object is: " ++ show v
    Right p -> return $ Right p
