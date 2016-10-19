---
title: Minimal json schema from json
tags: haskell, json-schema, tutorial
status: draft
---

# Script to convert json to minimal json-schema

Recently at work I had to build a tool to prevent misconfiguration of our apps. The
configuration file is a simple json file, but for added security, it was decided to
do some sanity checks on the values.

So the setup is simple: I have a fairly large json file and I want to output a corresponding
[json schema](http://json-schema.org/) to validate the config.

For example, given the following json object:

```json
{
  "network": {
    "poll_frequency": 42,
    "server_url": "https://foo.bar.com"
  },
  "storage": "sqlite"
}
```

Would give the following json schema:

```json
{
  "type": "object",
  "properties": {
    "network": {
      "type": "object",
      "properties": {
        "poll_frequency": {
          "type": "number"
        },
        "server_url": {
          "type": "string"
        }
      },
      "required": ["poll_frequency", "server_url"]
    },
    "storage": {
      "type": "string"
    }
  },
  "required": ["network", "storage"]
}
```


## Haskell script with stack
With stack, it is possible to run haskell as a script, which is very useful.
Some boilerplate first:

```haskell
#!/usr/bin/env stack
{- stack --resolver lts-6.10 runghc
    --package text
    --package aeson
    --package aeson-pretty
    --package containers
-}
```

A few notes:

* `#!/usr/bin/env stack` This is a traditional shabang to tell the system to use stack. Of course stack has to be
installed on the system for that to work.
The next lines are not haskell related, they tell stack what to do with this file.

* `stack --resolver lts-6.10 runghc` Specify a ghc version and instruct stack to run the file. If this version
is not installed, stack will download and install it.

* Following is a list of packages used for this script. All very standard things here.


## Pragmas and imports

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as B
import qualified System.Environment as Env
import qualified Data.Aeson as JSON
import Data.Aeson.Encode.Pretty (encodePretty', defConfig, confCompare)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Vector as Vector
```

[OverloadedStrings](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions#overloadedstrings) is very basic and makes working with `Text` and `String` much easier. I'll go over the imports as they are used in the code. I like to keep them qualified as much as possible to keep things crystal clear.


## Decode a given payload

```haskell
main :: IO ()
main = do
    [specPath, outPath] <- Env.getArgs
    rawSpec <- B.readFile specPath
    case JSON.eitherDecode rawSpec of -- :: Either String JSON.Value
        Left err -> print err
        Right val -> undefined -- coming soon
```

Aeson provides a function
[`eitherDecode`](http://hackage.haskell.org/package/aeson-0.11.2.0/docs/Data-Aeson.html#v:eitherDecode)
with the type `FromJSON a => ByteString -> Either String a`. Here, explicitely providing an instance is not required
because we're going to directly use a
[JSON Value](http://hackage.haskell.org/package/aeson-0.11.2.0/docs/Data-Aeson.html#t:Value) and not bother
converting to some internal datatype.


## Converting a json object to a schema
This is where pattern matching comes *very* handy.

The function will have a simple type:

```haskell
convertToSchema :: JSON.Value -> JSON.Value
```

First, let's have a look at the simple cases. Json schema has some
[primitive types](http://json-schema.org/latest/json-schema-core.html#anchor8), like `number`, `null`, `string` and `boolean`.

```haskell
convertToSchema (JSON.String _) = JSON.Object $ Map.singleton "type" (JSON.String "string")
convertToSchema (JSON.Number _) = JSON.Object $ Map.singleton "type" (JSON.String "integer")
convertToSchema (JSON.Bool _) = JSON.Object $ Map.singleton "type" (JSON.String "boolean")
convertToSchema JSON.Null = JSON.Object $ Map.singleton "type" (JSON.String "null")
```

Here, every value with a given type is simply replaced by a schema specifying its type.

Now, for an object, we want to do two things:

* Mark all properties as `required`
* Recursively call the function on the values of the object to convert them to schema definition

```haskell
convertToSchema (JSON.Object o) =
  let
    keys = JSON.String <$> Map.keys o  -- wrap all keys as json string
    props = JSON.Object $ convertToSchema <$> o  -- recurse over the values of the current object
  in
    JSON.Object $ Map.fromList [
      ("type", JSON.String "object")
    , ("properties", props)
    , ("required", JSON.Array $ V.fromList keys)
    ]
```

An array is very similar. The main difference is to chose which schema(s) to generate for the items.
Also, the minimum number of items will be the
number of items we got. This is somewhat arbitrary but it can always be changed later (manually if needs be).


```haskell
-- utility to remove duplicates in a vector
nubVector v = V.fromList $ Set.toList $ V.foldl' (flip Set.insert) Set.empty v

convertToSchema (JSON.Array arr) =
    let
        items = convertToSchema <$> arr
        uniqueItems = nubVector items
        innerSchema = if length uniqueItems == 1
            then uniqueItems V.! 0
            else JSON.Object $ Map.singleton "oneOf" (JSON.Array uniqueItems)
        minItems = fromIntegral (length arr)
    in
        JSON.Object $ Map.fromList [
        ("type", JSON.String "array")
        , ("minItems", JSON.Number minItems)
        , ("items", innerSchema)
        ]
```


## Pretty printing the schema
That's where `aeson-pretty` comes in handy:

```haskell
    -- continued from main
    Right val -> do
        let schema = convertToSchema val
        let pretty = encodePretty' (defConfig {confCompare = compare}) schema
        B.writeFile outPath pretty
```


Et voilà, nothing more to do.
The resulting schema is very minimal, but it's a good base to customize later. Since json schemas can be quite
verbose, it's a pain to manually write them.

For ease of copy pasting, the complete script can be found below.

```haskell
#!/usr/bin/env stack
{- stack --resolver lts-6.10 runghc
    --package text
    --package aeson
    --package aeson-pretty
    --package containers
    -- -W
-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as B
import qualified System.Environment as Env
import qualified Data.Aeson as JSON
import Data.Aeson.Encode.Pretty (encodePretty', defConfig, confCompare)
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V
import qualified Data.HashSet as Set

main :: IO ()
main = do
  [specPath, outPath] <- Env.getArgs
  rawSpec <- B.readFile specPath
  case JSON.eitherDecode rawSpec of -- :: Either String JSON.Value
    Left err -> print err
    Right val -> do
      let schema = convertToSchema val
      let pretty = encodePretty' (defConfig {confCompare = compare}) schema
      B.writeFile outPath pretty


convertToSchema :: JSON.Value -> JSON.Value
convertToSchema (JSON.Object o) =
  let
    keys = JSON.String <$> Map.keys o
    props = JSON.Object $ convertToSchema <$> o
  in
    JSON.Object $ Map.fromList [
      ("type", JSON.String "object")
    , ("properties", props)
    , ("required", JSON.Array $ V.fromList keys)
    ]

convertToSchema (JSON.Array arr) =
    let
        items = convertToSchema <$> arr
        uniqueItems = nubVector items
        innerSchema = if length uniqueItems == 1
            then uniqueItems V.! 0
            else JSON.Object $ Map.singleton "oneOf" (JSON.Array uniqueItems)
        minItems = fromIntegral (length arr)
    in
        JSON.Object $ Map.fromList [
        ("type", JSON.String "array")
        , ("minItems", JSON.Number minItems)
        , ("items", innerSchema)
        ]

convertToSchema (JSON.String _) = JSON.Object $ Map.singleton "type" (JSON.String "string")
convertToSchema (JSON.Number _) = JSON.Object $ Map.singleton "type" (JSON.String "integer")
convertToSchema (JSON.Bool _) = JSON.Object $ Map.singleton "type" (JSON.String "boolean")
convertToSchema JSON.Null = JSON.Object $ Map.singleton "type" (JSON.String "null")

-- remove duplicates in a vector
nubVector v = V.fromList $ Set.toList $ V.foldl' (flip Set.insert) Set.empty v
```
