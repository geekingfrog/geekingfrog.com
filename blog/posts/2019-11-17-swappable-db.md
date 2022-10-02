---
title: Swap the DB based on a config file
status: published
tags:
- haskell
- tutorial
---

# Swappable DB backend

So today, someone in `#haskell-beginners` asked a question about switching the database for an application
based on a config file. Let's see a fairly simple solution for this problem. This is meant to be an intermediate tutorial. You should be comfortable with newtypes, typeclasses, monads and perhaps a little bit of json handling but not much more.


# Step 1: define an interface for DB operations

First, let's define a typeclass for all database operations. The application will only use these operations
and this typeclass. We don't want to put anything specific to a database there since it should be swappable.

```haskell
class MonadDB m where
  -- | count the number of item for the given table/collection
  countItems :: String -> m Int
```

Only one method there to keep things simple, but you can add many more.

# Step 2: Create a simple application using this interface

Let's create a very simple application using that.

```haskell
compareSweets :: (MonadDB m, MonadIO m) => m ()
compareSweets = do
  numberOfCakes <- countItems "cake"
  numberOfCookies <- countItems "cookies"
  if numberOfCakes > numberOfCookies
    then putStrLn "There are more cakes than cookies in store"
    else putStrLn "Oh noooes, bake more cakes !"
```

# Step 3: Run the application with a postgresql backend

The basic application will be using the [readerT pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern).

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Reader as Rdr

-- from the package postgresql-simple
import qualified Database.PostgreSQL.Simple as PG

-- from the package safe-exceptions
import qualified Control.Exception.Safe as Exc

data PGEnv = PGEnv
  { pgConn :: PG.Connection
  }

newtype PostgresApp m a = PostgresApp { runPostgresApp :: ReaderT PGEnv m a }
  deriving (Functor, Applicative, Monad, MonadReader PGEnv, MonadIO, Exc.MonadThrow)
  -- ^ we get all of that for free thanks to GeneralizedNewtypeDeriving
```
A simple wrapper to get access to a persistent connection to the database. This
could also be a connection pool, or whatever is needed.

Now, we need to declare an instance of `MonadDB` for this application.

```haskell
instance (MonadIO m, Exc.MonadThrow m) => MonadDB (PostgresApp m) where
  countItems tableName = do
    conn <- Rdr.asks pgConn
    result <- liftIO $ PG.query conn "select * from ?" (PG.Only tableName)
    case result of
      [PG.Only count] -> pure count
      _ -> Exc.throwString $ "countItems blew up for table: " <> tableName

```

And now, let's run all of that:

```haskell
main :: IO ()
main = do
  -- connectToPostgresql is easy to write, we'll see later how to improve it
  connection <- connectToPostgresql
  let pgEnv = PGEnv { pgConn = connection }
  Rdr.runReaderT (runPostgresApp compareSweets) pgEnv
```


# Step 4: Get a config file in place

So far, everything was hardcoded. Let's introduce a simple config file in json format.

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson as Aeson
import GHC.Generics
import GHC.Word

-- the basic config to read from a file
data PGConfig = PGConfig
  { host :: String
  , port :: Word16
  , user :: String
  , password :: String
  , database :: String
  }
  deriving (Show, Generic, Aeson.FromJSON)
```
The two extensions `DeriveAnyClass` and `DeriveGeneric` allow us to get a free `FromJSON` instance.
Here's an example of a simple json file:

```json
{
  "host": "localhost",
  "port": 5432,
  "user": "postgres",
  "password": "password123",
  "database": "cake-factory"
}
```

Let's write some simple code to make sure everything is working as expected:

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

main :: IO ()
main = do
  (config :: PGConfig) <- Aeson.eitherDecodeFileStrict "./config.json" >>= \case
    Left err -> Exc.throwString $ "Cannot decode config file: " <> err
    Right x -> pure x
  print config
```

`LambdaCase` is my favorite extension, it allows to do a `case ... of` without having to conjure a name
just for the sake of matching on it. `ScopedTypeVariables` is required there to let GHC know what
is the type of the thing we're trying to decode.

Now, let's plug the configuration in the application:

```haskell
mkPGEnv :: PGConfig -> IO PGEnv
mkPGEnv conf = do
  conn <- PG.connect $ PG.ConnectInfo
    { PG.connectHost = host conf
    , PG.connectPort = port conf
    , PG.connectUser = user conf
    , PG.connectPassword = password conf
    , PG.connectDatabase = database conf
    }
  pure $ PGEnv conn

main :: IO ()
main = do
  config <- Aeson.eitherDecodeFileStrict "./config.json" >>= \case
    Left err -> Exc.throwString $ "Cannot decode config file: " <> err
    Right x -> pure x
  env <- mkPGEnv config
  Rdr.runReaderT (runPostgresApp compareSweets) env
```


# Step 5: Extend the configuration

Now, to connect to another DB based on the configuration, we need to extend the config a little bit:

```haskell
data DBConfig
  = DBConfigPostgres PGConfig
  | DBConfigMem -- imaginary in memory config

instance Aeson.FromJSON DBConfig where
  parseJSON raw = Aeson.withObject "DBConfig" (\o -> do
    dbType <- o .: "type" -- (.:) is coming from Aeson
    case dbType of
      "postgres" -> do
        conf <- parseJSON raw
        pure $ DBConfigPostgres conf
      "memory" -> pure DBConfigMem
      _ -> fail $ "Unknown DB type: " <> dbType
    ) raw
```
The config file now must have a field `type` to specify the database to use. The other fields are the one
required for the specific DB.

# Step 6: choose the implementation based on the config

```haskell
main :: IO ()
main = do
  config <- Aeson.eitherDecodeFileStrict "./config.json" >>= \case
    Left err -> Exc.throwString $ "Cannot decode config file: " <> err
    Right x -> pure x
  case config of
    DBConfigPostgres pgConf -> do
      env <- mkPGEnv pgConf
      Rdr.runReaderT (runPostgresApp compareSweets) env
    DBConfigMem -> error "no backend for in memory db yet"
```


# Conclusion
This is a fairly straightforward way to have multiple swappable DB for an haskell application.
It's also pretty easy to test as a result, just use an in-memory DB.
