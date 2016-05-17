{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Geekingfrog.Queries where

import Data.Text (Text(..))
import Safe (headMay)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (liftA)

import Database.Persist
import Database.Persist.Sqlite (runSqlite)
import Database.Esqueleto as E

import Geekingfrog.Db.Types

-- Generic type to restrict the query for the join query
type PostTagQuery = SqlExpr (Entity Post) -> SqlExpr (Entity Tag) -> SqlExpr (Entity PostTag) -> SqlQuery ()

-- Generic function which does a join between posts and tags.
-- The join can be further restricted with the given query
getPostsAndTags :: PostTagQuery -> IO [(Entity Post, Entity Tag)]
getPostsAndTags query = runSqlite "testing.sqlite" $ select $
    from $ \((post `InnerJoin` postTag) `InnerJoin` tag) -> do
      on $ tag ^. TagId E.==. postTag ^. PostTagTagId
      on $ post ^. PostId E.==. postTag ^. PostTagPostId
      query post tag postTag
      return (post, tag)


getAllPostsAndTags :: IO [(Entity Post, [Entity Tag])]
getAllPostsAndTags = do
  let query post _ _ = do
        E.orderBy [E.asc (post ^. PostPublishedAt)]
        E.where_ (E.not_ $ E.isNothing $ post ^. PostPublishedAt)
  liftIO $ liftA groupPostTags (getPostsAndTags query)

getLastPostTags :: IO [(Entity Post, [Entity Tag])]
getLastPostTags = do
  let subPosts = subList_select $ from $ \p -> do
                    where_ (not_ $ isNothing $ p ^. PostPublishedAt)
                    limit 5
                    orderBy [desc (p ^. PostPublishedAt)]
                    return $ p ^. PostId

  let query post _ _ = do
        where_ $ post ^. PostId `in_` subPosts
        orderBy [asc (post ^. PostPublishedAt)]

  liftA groupPostTags (getPostsAndTags query)

getOnePostAndTags :: Text -> IO (Maybe (Entity Post, [Entity Tag]))
getOnePostAndTags postSlug = do
  postAndTags <- getPostsAndTags query
  let tags = map snd postAndTags
  case headMay postAndTags of
    Nothing -> return Nothing
    Just (post, _) -> return $ Just (post, tags)
  where query post _ _ = where_ (post ^. PostSlug E.==. val postSlug)

getPostBySlug slug = runSqlite "testing.sqlite" $ getBy $ UniquePostSlug slug

-- assume sorted by DB.Post
groupPostTags :: [(Entity Post, Entity Tag)] -> [(Entity Post, [Entity Tag])]
groupPostTags = go []
  where go acc [] = acc
        go [] ((p, t):xs) = go [(p, [t])] xs
        go ((p, tags):rest) ((post, tag):xs) = if entityKey p == entityKey post
                                               then go ((p, tag:tags):rest) xs
                                               else go ((post, [tag]):(p, tags):rest) xs
