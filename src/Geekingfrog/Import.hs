{-# LANGUAGE OverloadedStrings #-}

module Geekingfrog.Import where

import Database.Persist
import Database.Persist.Sqlite (runSqlite, runMigration)

import Control.Monad.IO.Class (liftIO)
import Data.DateTime (fromSeconds)
import Data.Time.Clock (getCurrentTime)
import Control.Applicative (liftA)
import Control.Monad (zipWithM_)

import qualified Geekingfrog.Db.Types as DT
import Geekingfrog.Types

testPersistent :: IO ()
testPersistent = runSqlite ":memory:" $ do
-- testPersistent = runSqlite "testing.sqlite" $ do
  runMigration DT.migrateAll
  -- now <- liftIO getCurrentTime
  -- let tagId = TagKey 1
  -- let tag = Tag "testUUID" "testName" "test-slug" (Just "test description") False now 1 now 1
  -- insertKey tagId tag
  -- testtag <- getBy $ UniqueSlug "test-slug"
  -- liftIO $ putStrLn $ "printing tag: " ++ show testtag
  -- let foo = liftA (tagName . entityVal) testtag
  -- liftIO $ putStrLn "---"
  liftIO $ putStrLn "all is well"

importUsers :: [User] -> IO ()
importUsers users = runSqlite "testing.sqlite" $ do
  runMigration DT.migrateAll
  let dbUsers = fmap userToDb users
  let userIds = fmap (DT.UserKey . userId) users
  zipWithM_ insertKey userIds dbUsers
  liftIO $ putStrLn $ "Exported " ++ show (length users) ++ " users"


userToDb :: User -> DT.User
userToDb usr = DT.User (userUuid usr)
                       (userName usr)
                       (userSlug usr)
                       (userPassword usr)
                       (userEmail usr)
                       (userLastLogin usr)
                       (userCreatedAt usr)
