{-# LANGUAGE OverloadedStrings #-}

import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.Directory as Dir
import qualified Data.ByteString as B
import qualified Control.Monad as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid ((<>))

import qualified Data.DateTime as D

import qualified Geekingfrog.Parse as Parse
import qualified Geekingfrog.GhostTypes as Types --(Post, Tag, PostTag)


main = do
  filename <- getImportFile
  rawContent <- B.readFile filename
  let ghostExport = Parse.parseGhostExport rawContent
  Dir.createDirectoryIfMissing False "posts/"
  case ghostExport of
    Left parseError -> putStrLn parseError
    Right (errors, (posts, tags, postTags)) -> M.mapM_ (savePost tags postTags) posts


getImportFile :: IO String
getImportFile = do
  args <- Env.getArgs
  progName <- Env.getProgName
  if null args
  then Exit.die $ "usage: " ++ show progName ++ " <ghost-export.json>"
  else do
    let filename = head args
    fileExists <- Dir.doesFileExist filename
    if fileExists
    then return filename
    else Exit.die $ show filename ++ " not found"


savePost :: [Types.Tag] -> [Types.PostTag] -> Types.Post -> IO ()
savePost tags postTags post = do
  let (year, month, day) = D.toGregorian' $ Types.postCreatedAt post
  let ts = getTagsForPost tags postTags post
  let prefix = T.pack (show year) <> "-" <> formatDate month <> "-" <> formatDate day
  let cleanPost = T.replace "`" "" (Types.postSlug post)
  let fileName = prefix <> "-" <> cleanPost
  let filePath = "posts/" <> fileName
  let header = T.intercalate "\n"
        [ "---"
        , "title: " <> Types.postTitle post
        , "tags: " <> T.intercalate ", " (map Types.tagSlug ts)
        , "status: " <> if isJust (Types.postPublishedAt post) then "published" else "draft"
        , "---"
        , "\n"
        ]
  let content = Types.postMarkdown post
  T.writeFile (T.unpack filePath <> ".md") (header <> content)


formatDate :: Int -> T.Text
formatDate d = if d < 10 then "0" <> T.pack (show d) else T.pack (show d)

getTagsForPost :: [Types.Tag] -> [Types.PostTag] -> Types.Post -> [Types.Tag]
getTagsForPost tags postTags post =
  let
    postId = Types.postId post
    pt = map Types.postTagTagId $ filter ((== postId) . Types.postTagPostId) postTags
    filteredTags = filter (\t -> Types.tagId t `elem` pt) tags
  in
    filteredTags
