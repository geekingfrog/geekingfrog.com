{-# LANGUAGE OverloadedStrings #-}

import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.Directory as Dir
import qualified Data.ByteString as B
import qualified Control.Monad as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import qualified Data.DateTime as D

import qualified Geekingfrog.Parse as Parse
import qualified Geekingfrog.Types as Types --(Post, Tag, PostTag)


main = do
  filename <- getImportFile
  rawContent <- B.readFile filename
  let ghostExport = Parse.parseGhostExport rawContent
  Dir.createDirectoryIfMissing False "posts/"
  case ghostExport of
    Left parseError -> putStrLn parseError
    Right (errors, (posts, tags, _)) -> M.mapM_ (savePost tags) posts
  -- M.mapM_ 
  print "yo"


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

savePost :: [Types.Tag] -> Types.Post -> IO ()
savePost tags post = do
  let (year, month, day) = D.toGregorian' $ Types.postCreatedAt post
  let prefix = T.pack (show year) <> "-" <> T.pack (show month) <> "-" <> T.pack (show day)
  let cleanPost = T.replace "`" "" (Types.postSlug post)
  let fileName = prefix <> "-" <> cleanPost
  T.putStrLn fileName

  putStrLn $ T.unpack fileName
  let filePath = "posts/" <> fileName
  let header = T.intercalate "\n"
        [ "---"
        , "title: " <> Types.postTitle post
        , "description: " <> fromMaybe "" (Types.postMetaDescription post)
        , "tags: " <> " TODO put tags here"
        , "---"
        , "\n"
        ]
  T.writeFile (T.unpack filePath) header
