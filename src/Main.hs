{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.DateTime (getCurrentTime)
import Servant

import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)

import Network.HTTP.Types (status404)

import qualified Data.List as List
import qualified Control.Monad as M
import Control.Monad.IO.Class (liftIO)
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified Data.HashMap.Strict as Map

import Geekingfrog.AtomFeed (AtomFeed(..))
import Geekingfrog.ContentType (XML)
import Geekingfrog.Constants (siteUrl, highlightStyle)

import qualified Geekingfrog.Views.Errors as Errors
import qualified Geekingfrog.Urls as Urls
import qualified Geekingfrog.Queries as Queries
import qualified Geekingfrog.Types as Types

import qualified Geekingfrog.Views as Views
import qualified Geekingfrog.HtmlApi as HtmlApi
import qualified Geekingfrog.JSONApi as JSONApi

import qualified Geekingfrog.MarkdownParser as MdParser
import qualified Text.Highlighting.Kate.Styles as Highlighting
import qualified Text.Highlighting.Kate.Format.HTML as Highlighting

main :: IO ()
main = let port = 8080 in do
  -- generateSitemap
  putStrLn $ "Listening on port " ++ show port ++ "..."
  testingPost <- Text.readFile "./posts/2016-04-10-struggles-with-parsing-json-with-aeson"
  createHighlightCss highlightStyle
  postMap <- loadPosts
  case postMap of
    Left err -> Exit.die (show err)
    Right postMap' -> run port (app postMap')


type WebsiteAPI =
  HtmlApi.HtmlAPI
  :<|> "rss" :> Get '[AtomFeed, XML] AtomFeed
  :<|> "robots.txt" :> Get '[PlainText] Text
  :<|> "sitemap.txt" :> Get '[PlainText] Text
  :<|> "static" :> Raw -- staticServer
  :<|> "admin" :> Raw -- admin spa
  :<|> "api" :> JSONApi.JsonAPI
  :<|> Raw  -- catchall for custom 404

websiteApi :: Proxy WebsiteAPI
websiteApi = Proxy

websiteServer :: Types.PostMap -> Server WebsiteAPI
websiteServer postMap = HtmlApi.htmlHandler postMap
           :<|> makeFeed postMap
           :<|> serveRobots
           :<|> serveSitemap
           :<|> serveDirectory "./static"
           :<|> serveDirectory "./admin"
           :<|> JSONApi.apiHandler
           :<|> custom404

app :: Types.PostMap -> Application
app postMap = serve websiteApi (websiteServer postMap)

makeFeed :: Types.PostMap -> Handler AtomFeed
makeFeed postMap = do
  now <- liftIO getCurrentTime
  let posts = Map.elems postMap
  let publishedPost = filter ((==Types.Published) . Types.postStatus) posts
  let sortedPosts = reverse $ List.sortOn Types.postCreatedAt publishedPost
  return $ AtomFeed now (take 10 sortedPosts)


-- Network.Wai.Request -> (Network.Wai.Response -> IO ResponseReceived) -> ResponseReceived
custom404 :: Application
custom404 _ sendResponse = sendResponse $ responseLBS status404
                             [("Content-Type", "text/html; charset=UTF-8")]
                             (renderMarkup Errors.notFound)

serveRobots :: Handler Text
serveRobots = serveFile "./robots.txt"

serveSitemap :: Handler Text
serveSitemap = serveFile "./sitemap.txt"

serveFile :: FilePath -> Handler Text
serveFile filepath = do
  exists <- liftIO (Dir.doesFileExist filepath)
  if exists
    then liftIO (Text.readFile filepath)
    else throwError err404

-- generateSitemap :: IO ()
-- generateSitemap = do
--   posts <- liftIO Queries.getPublishedPostsAndTags
--   let fixedUrls = [
--             -- not completely accurate, but for urlFor this doesn't matter
--             Urls.urlFor $ Views.Index posts
--           , Urls.urlFor Views.GpgView
--           , Urls.urlFor $ Views.PostsOverview posts
--         ]
--   let postsUrls = fmap (Urls.urlFor . Views.PostView) posts
--   let urls = fmap (Text.append siteUrl) (fixedUrls ++ postsUrls)
--   Text.writeFile "./sitemap.txt" (Text.unlines urls)


loadPosts :: IO (Either String (Map.HashMap Text Types.Post))
loadPosts = do
  postList <- Dir.listDirectory "posts"
  contents <- M.forM postList (\filename -> Text.readFile ("posts/" ++ filename))
  let metas = M.mapM (MdParser.parsePostFileName . Text.pack) postList
  let postContents = M.mapM MdParser.parsePost (zip postList contents)
  case (metas, postContents) of
    (Left err, _) -> return . Left $ err
    (_, Left err) -> return . Left $ err
    (Right okMetas, Right okContents) -> do
      let posts = zipWith makePost okMetas okContents
      return . Right $ Map.fromList posts


makePost :: MdParser.PostMeta -> MdParser.PostContent -> (Text, Types.Post)
makePost (date, slug) (title, status, markdown, html, tags) = (slug, Types.Post {
            Types.postStatus = status
          , Types.postTitle = title
          , Types.postSlug = slug
          , Types.postMarkdown = markdown
          , Types.postHtml = html
          , Types.postCreatedAt = date
          , Types.postTags = tags
          })

createHighlightCss style = writeFile "static/highlight.css" (Highlighting.styleToCss style)
