{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)
import System.Exit (die)
import Database.Persist
import Database.Persist.Sqlite as SQL

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (zipWithM_, when)
import Control.Monad.Logger --(runStderrLoggingT)
import qualified Data.ByteString as B (readFile)

import qualified Geekingfrog.Db.Types as DT
import qualified Geekingfrog.Db.PostStatus as DT
import Geekingfrog.Types
import Geekingfrog.Parse (parseGhostExport)

import Control.Monad.Trans.Reader (ReaderT)

main = do
  filename <- getImportFile
  putStrLn $ "Importing ghost export from: " ++ show filename
  rawContent <- B.readFile filename
  let ghostExport = parseGhostExport rawContent
  case ghostExport of
    Left err -> do
      putStrLn "Parse error when importing ghost archive"
      print err
    Right (errors, (posts, tags, postTags)) -> do
      when (null errors) $ putStrLn ("got some (non fatal) errors while parsing: " ++ show errors)
      importData tags posts postTags
  putStrLn "Everything imported"

getImportFile :: IO String
getImportFile = do
  args <- getArgs
  progName <- getProgName
  if null args
  then die $ "usage: " ++ show progName ++ " <ghost-export.json>"
  else do
    let filename = head args
    fileExists <- doesFileExist filename
    if fileExists
    then return filename
    else die $ show filename ++ " not found"

importData :: [Tag] -> [Post] -> [PostTag] -> IO ()
importData tags posts postTags = runNoLoggingT $  -- runStderrLoggingT
  SQL.withSqlitePool "testing.sqlite" 10 $ \pool -> liftIO $
    flip SQL.runSqlPersistMPool pool $ do
      SQL.runMigration DT.migrateAll
      importTags tags
      importPosts posts
      importPostTags postTags
      liftIO . putStrLn $ "All imported"

importTags :: (MonadIO m) => [Tag] -> ReaderT SqlBackend m ()
importTags tags = do
  let tagIds = fmap (DT.TagKey . tagId) tags
  let dbTags = fmap tagToDb tags
  zipWithM_ repsert tagIds dbTags
  liftIO . putStrLn $ "Successfully imported " ++ show (length tags) ++ " tags"

importPosts :: (MonadIO m) => [Post] -> ReaderT SqlBackend m ()
importPosts posts = do
  let postIds = fmap (DT.PostKey . postId) posts
  let dbPosts = fmap postToDb posts
  zipWithM_ repsert postIds dbPosts
  liftIO . putStrLn $ "Successfully imported " ++ show (length posts) ++ " posts"

importPostTags :: (MonadIO m) => [PostTag] -> ReaderT SqlBackend m ()
importPostTags postTags = do
  let ids = fmap (DT.PostTagKey . postTagId) postTags
  let dbPostTags = fmap postTagToDb postTags
  zipWithM_ repsert ids dbPostTags
  liftIO . putStrLn $ "Successfully imported " ++ show (length postTags) ++ " post-tags relations"

tagToDb :: Tag -> DT.Tag
tagToDb tag = DT.Tag (tagUuid tag)
                     (tagName tag)
                     (tagSlug tag)
                     (tagDescription tag)
                     (tagHidden tag)
                     (tagCreatedAt tag)

postStatusToDb :: PostStatus -> DT.PostStatus
postStatusToDb Published = DT.Published
postStatusToDb Draft     = DT.Draft

postToDb :: Post -> DT.Post
postToDb post = DT.Post (postStatusToDb $ postStatus post)
                        (postUuid post)
                        (postTitle post)
                        (postSlug post)
                        (postMarkdown post)
                        (postHtml post)
                        (postCreatedAt post)
                        (postUpdatedAt post)
                        (postPublishedAt post)
                        (postLanguage post)
                        (postIsFeatured post)

postTagToDb :: PostTag -> DT.PostTag
postTagToDb pt = DT.PostTag (DT.TagKey $ postTagTagId pt)
                            (DT.PostKey $ postTagPostId pt)
                            (postTagSortOrder pt)
