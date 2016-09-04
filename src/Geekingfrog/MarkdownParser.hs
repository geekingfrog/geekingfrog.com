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
import Text.Megaparsec.Text
import qualified Text.Highlighting.Kate.Styles as Highlighting

import qualified Geekingfrog.Types as Types

type PostMeta = (Types.SimpleDate, Text) -- date, slug
-- title, status, markdown, html, tags
type PostContent = (Text, Types.PostStatus, Text, Html, [Types.Tag])
type PostHeader = (Text, Types.PostStatus, [Types.Tag])  -- title, status


parsePost :: (FilePath, Text) -> Either String PostContent
parsePost (fileName, raw) = case runParser postParser fileName raw of
  Left err -> Left $ parseErrorPretty err
  Right stuff -> postMarkdownToHtml stuff


postParser :: Parsec Dec Text (Text, Types.PostStatus, Text, [Types.Tag])
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


parsePostFileName :: Text -> Either String PostMeta
parsePostFileName = liftLeft parseErrorPretty . runParser metaPostParser "rawMetaPost"

metaPostParser :: Parsec Dec Text PostMeta
metaPostParser = do
  year <- read <$> someTill digitChar (char '-')
  month <- read <$> someTill digitChar (char '-')
  day <- read <$> someTill digitChar (char '-')
  slug <- T.pack <$> someTill anyChar eof
  return ((year, month, day), slug)


postMarkdownToHtml :: (Text, Types.PostStatus, Text, [Types.Tag]) -> Either String PostContent
postMarkdownToHtml (title, status, markdown, tags) = case markdownToHtml (T.unpack markdown) of
  Left err -> Left err
  Right html -> Right (title, status, markdown, html, tags)


markdownToHtml markdown = case Pandoc.readMarkdown options markdown of
  Left pandocError -> Left (show pandocError)
  Right doc -> Right $ Pandoc.writeHtml writeOptions doc
  where
    options = Pandoc.def {Pandoc.readerExtensions = Pandoc.githubMarkdownExtensions}
    writeOptions = Pandoc.def
      { Pandoc.writerHighlight = True
      , Pandoc.writerHighlightStyle = Highlighting.zenburn
      }


liftLeft :: (a -> b) -> Either a c -> Either b c
liftLeft f (Left err) = Left (f err)
liftLeft _ (Right stuff) = Right stuff
