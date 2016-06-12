{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Geekingfrog.Queries where

import Data.Text (Text(..))
import Safe (headMay)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (liftA)
import Data.Maybe (catMaybes)

import Database.Persist
import Database.Persist.Sqlite (runSqlite)
import Database.Esqueleto as E

import Geekingfrog.Db.Types
import Geekingfrog.Db.PostStatus

-- Generic type to restrict the query for the join query
type PostTagQuery =
     SqlExpr (Entity Post)
  -> SqlExpr (Maybe (Entity Tag))
  -> SqlExpr (Maybe (Entity PostTag))
  -> SqlQuery ()

-- Generic function which does a join between posts and tags.
-- The join can be further restricted with the given query
getPostsAndTags :: PostTagQuery -> IO [(Entity Post, Maybe (Entity Tag))]
getPostsAndTags query = runSqlite "testing.sqlite" $ select $
    from $ \((post `LeftOuterJoin` postTag) `LeftOuterJoin` tag) -> do
      on $ tag ?. TagId E.==. postTag ?. PostTagTagId
      on $ just (post ^. PostId) E.==. postTag ?. PostTagPostId
      query post tag postTag
      return (post, tag)

getAllPostsAndTags :: IO [(Entity Post, [Entity Tag])]
getAllPostsAndTags = do
  let query post _ _ = E.orderBy [E.desc (post ^. PostUpdatedAt),  E.asc (post ^. PostStatus)]
  liftIO $ liftA groupEntity (getPostsAndTags query)

getPublishedPostsAndTags :: IO [(Entity Post, [Entity Tag])]
getPublishedPostsAndTags = do
  let query post _ _ = do
        E.orderBy [E.asc (post ^. PostPublishedAt)]
        E.where_ (post ^. PostStatus E.==. E.val Published)
  liftIO $ liftA groupEntity (getPostsAndTags query)

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

  liftA groupEntity (getPostsAndTags query)

getOnePostAndTags :: Text -> IO (Maybe (Entity Post, [Entity Tag]))
getOnePostAndTags postSlug = do
  postAndTags <- getPostsAndTags query
  let tags = map snd postAndTags
  case headMay postAndTags of
    Nothing -> return Nothing
    Just (post, _) -> return $ Just (post, catMaybes tags)
  where query post _ _ = where_ (post ^. PostSlug E.==. val postSlug)

getPostBySlug slug = runSqlite "testing.sqlite" $ getBy $ UniquePostSlug slug

getTags :: IO [(Entity Tag, [Entity PostTag])]
getTags = liftA groupEntity $ runSqlite "testing.sqlite" $ select $
    from $ \(tag `LeftOuterJoin` postTag) -> do
      on $ just (tag ^. TagId) E.==. postTag ?. PostTagTagId
      E.orderBy [E.asc (tag ^. TagSlug), E.asc (tag ^. TagCreatedAt)]
      return (tag, postTag)


-- assume sorted by a
groupEntity :: (Eq (Key a)) => [(Entity a, Maybe (Entity b))] -> [(Entity a, [Entity b])]
groupEntity = map (\(post, tags) -> (post, catMaybes tags)) . go []
  where go acc [] = acc
        go [] ((a, b):xs) = go [(a, [b])] xs
        go ((a, bs):rest) ((a', b'):xs) =
          if entityKey a == entityKey a'
          then go ((a, b':bs):rest) xs
          else go ((a', [b']):(a, bs):rest) xs
