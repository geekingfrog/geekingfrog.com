---
title: Struggles with parsing JSON with Aeson
status: published
tags:
- haskell
- aeson
---

[Aeson](http://hackage.haskell.org/package/aeson) is the de-facto library to get data to and from json. It works very well but there is a lot of magic and I had some issue tweaking the default behaviors, notably to add some more error messages.

# Simple usecases
Let's start with an very simple example, to show the strength and ease of use for normal usecases.

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- the main aeson imports
import Data.Aeson
import Data.Aeson.Types

import Data.Text
import qualified Data.ByteString.Lazy as B

-- A simple datatype
data Person = Person {
    personId :: Text
  , personName :: Text
  } deriving (Show, Generic)

instance FromJSON Person where
  parseJSON = withObject $ \o -> Person <$> o .: "id" <*> o .: "name"
```

Now, given a simple json file:
```json
{
  "id": "1",
  "name": "Boris"
}
```

The parsing is straightforward:
```haskell
main = do
  raw <- B.readFile "onePerson.json"
  print $ eitherDecode raw :: Either String Person
```

# When things go wrong
With `eitherDecode`, it's always possible to get some error message. For example:

```json
[{"id": "1", "name": "Boris"}]
```
Will yield `Left "Error in $: expected person, encountered Array"`. A missing key also will raise an error: `{"id": "1"}` yields `Left "Error in $: key \"name\" not present"`.

# Extending the error messages
Now, I had to import a fairly big array of json, and one of the record was invalid. The overall error was just about an invalid key type, but I had no idea where the error was. So the next task is to have more verbose logging in case things go wrong (and they will do, always).

## For a single object
What we want is an improved parser of type `Value -> Parser (Either String Person)` where the error message will be more verbose.

```haskell
verboseParser :: Value -> Parser (Either String Person)
verboseParser v = do
  case parseEither parseJSON v of
    Left err -> return . Left $ err ++ " -- Invalid object is: " ++ show v
    Right parsed -> return $ Right parsed
```

And then, we can use this parser:
```haskell
import Control.Monad (join)

main = do
  raw <- B.readFile "onePerson.json"
  print $ join . parseEither verboseParser raw
```

Running this example with `{"id": "1"}` will gives:
```haskell
Left "Error in $: key \"name\" not present -- Invalid object is: Object (fromList [(\"id\",String \"1\")])"
```

## For an array of objects

Now, we want to use this verbose parser for an array of objects. We can't rely on the default `FromJSON` instances for an array, so we have to manually specify and run the parser:

```haskell
import Data.Vector (toList)

main = do
  raw <- B.readFile "manyPeople.json"
  print $ eitherDecode raw >>= parseEither verboseParseMany

verboseParseMany :: Value -> Parser [Either String Person]
verboseParseMany = withArray "people" $ \arr -> do
  let allParsed = fmap (join . parseEither verboseParser) arr
  return $ toList allParsed
```

# Bonus: with nested structure
Let's assume the json is not a plain array, but something like:
```javascript
{
  "data": [{"id": "1", "name": "Boris"}]
}
```

We wrap the previous parser with another one:
```haskell
verboseParseMany' = withObject "response" $ \o -> do
  content <- o .: "data"  -- extract the interesting part
  verboseParseMany content  -- use the previous parser
```
And then, this new parser can be used wherever the previous one was used.

# Conclusion
Aeson has a lot of nice default, but understanding all the magic to extend the default behaviors can be hard at first. I believe this set of examples is enough to tackle a lot of usecase. Another great resource to understand Aeson magic is [this other aeson tutorial](https://artyom.me/aeson). The official aeson doc is great too.
