{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.MarkdownParser (
    parsePost
  , parsePostFileName
  , PostMeta
  , PostContent
) where

import qualified Data.List.NonEmpty as NonEmpty

import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Options as Pandoc
import Text.Blaze.Html (Html)
import Control.Applicative ((<*))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Skylighting.Styles as Highlighting

import qualified Geekingfrog.Types as Types

type Parser = Parsec Void Text

type PostMeta = (Types.SimpleDate, Text) -- date, slug
-- title, status, markdown, html, tags
type PostContent = (Text, Types.PostStatus, Text, Html, [Types.Tag])
type PostHeader = (Text, Types.PostStatus, [Types.Tag])  -- title, status


parsePost :: (FilePath, Text) -> Either String PostContent
parsePost (fileName, raw) = case runParser postParser fileName raw of
  Left err -> Left $ errorBundlePretty err
  Right stuff -> postMarkdownToHtml stuff


postParser :: Parser (Text, Types.PostStatus, Text, [Types.Tag])
postParser = do
  (title, status, tags) <- parseHeader
  markdown <- parseMarkdown
  return (title, status, markdown, tags)


parseHeader :: Parser PostHeader
parseHeader = do
  manyTill (char '-') eol
  title <- T.pack <$> (string "title:" >> optional space >> manyTill anySingle eol)
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


parseStatus :: Parser Types.PostStatus
parseStatus = do
  string "status:" >> skipMany (char ' ')
  status <- (string "published" >> return Types.Published) <|> (string "draft" >> return Types.Draft)
  skipMany (char ' ')
  eol
  return status

parseMarkdown :: Parser Text
parseMarkdown = do
  skipMany spaceChar
  T.pack <$> many anySingle


parsePostFileName :: Text -> Either String PostMeta
parsePostFileName filename = liftLeft errorBundlePretty
  $ runParser metaPostParser ("rawMetaPost - " ++ T.unpack filename) filename

metaPostParser :: Parser PostMeta
metaPostParser = do
  year <- read <$> someTill digitChar (char '-')
  month <- read <$> someTill digitChar (char '-')
  day <- read <$> someTill digitChar (char '-')
  slug <- T.pack <$> someTill anySingle eof
  return ((year, month, day), slug)


postMarkdownToHtml :: (Text, Types.PostStatus, Text, [Types.Tag]) -> Either String PostContent
postMarkdownToHtml (title, status, markdown, tags) = case markdownToHtml markdown of
  Left err -> Left err
  Right html -> Right (title, status, markdown, html, tags)


markdownToHtml markdown = liftLeft show $ Pandoc.runPure
  (Pandoc.readMarkdown options markdown >>= Pandoc.writeHtml5 writeOptions)

  where
    options = Pandoc.def {Pandoc.readerExtensions = Pandoc.githubMarkdownExtensions}
    writeOptions = Pandoc.def
      { Pandoc.writerHighlightStyle = Just Highlighting.zenburn
      }

liftLeft :: (a -> b) -> Either a c -> Either b c
liftLeft f (Left err) = Left (f err)
liftLeft _ (Right stuff) = Right stuff
