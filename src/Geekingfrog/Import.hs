{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Geekingfrog.Import where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (UTCTime)
import Data.DateTime (fromSeconds)

-- import Database.Persist.TH (
--     share
--   , mkPersist
--   , sqlSettings
--   , mkMigrate
--   , persistLowerCase
--   )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Tag
  name String
  slug String
  Slug slug
  created_at UTCTime
  deriving Show
|]


testPersistent :: IO ()
testPersistent = runSqlite ":memory:" $ do
-- testPersistent = runSqlite "testing.sqlite" $ do
  runMigration migrateAll
  testtagid <- insert $ Tag "testTag" "test-tag" (fromSeconds 1414670534559)
  testtag <- getBy $ Slug "test-tag"
  liftIO $ print testtag
