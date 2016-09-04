{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.MarkdownParser (
    parsePost
  , parsePostFileName
  , PostMeta
  , PostContent
) where

import qualified Data.List.NonEmpty as NonEmpty

import Control.Applicative ((<*))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Text

import qualified Geekingfrog.Types as Types

type PostMeta = (Types.SimpleDate, Text) -- date, slug
type PostContent = (Text, Types.PostStatus, Text, [Types.Tag])  -- title, status, markdown, tags
type PostHeader = (Text, Types.PostStatus, [Types.Tag])  -- title, status

parsePost :: (FilePath, Text) -> Either (ParseError (Token Text) Dec) PostContent
parsePost (fileName, raw) = runParser postParser fileName raw

postParser :: Parsec Dec Text PostContent
postParser = do
  (title, status, tags) <- parseHeader
  markdown <- parseMarkdown
  return (title, status, markdown, tags)


parseHeader :: Parsec Dec Text PostHeader
parseHeader = do
  manyTill (char '-') eol
  title <- T.pack <$> (string "title:" >> optional space >> manyTill anyChar eol)
  tags <- parseTags
  many eol
  status <- parseStatus
  manyTill (char '-') eol
  return (title, status, tags)


parseTags = do
  string "tags:"
  skipMany (char ' ')
  (eol >> return []) <|> (tag `sepBy` char ',' <* eol)

tag = do
  skipMany $ char ' '
  t <- many (alphaNumChar <|> char '-')
  skipMany $ char ' '
  return Types.Tag {Types.tagName = T.pack t}


parseStatus :: Parsec Dec Text Types.PostStatus
parseStatus = do
  string "status:" >> skipMany (char ' ')
  status <- (string "published" >> return Types.Published) <|> (string "draft" >> return Types.Draft)
  skipMany (char ' ')
  eol
  return status

parseMarkdown :: Parsec Dec Text Text
parseMarkdown = do
  skipMany spaceChar
  T.pack <$> many anyChar


parsePostFileName :: Text -> Either (ParseError (Token Text) Dec) PostMeta
parsePostFileName = runParser metaPostParser "rawMetaPost"

metaPostParser :: Parsec Dec Text PostMeta
metaPostParser = do
  year <- read <$> someTill digitChar (char '-')
  month <- read <$> someTill digitChar (char '-')
  day <- read <$> someTill digitChar (char '-')
  slug <- T.pack <$> someTill anyChar eof
  return ((year, month, day), slug)
