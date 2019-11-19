---
title: Deriving magic and parsing csv
tags: haskell, tutorial
status: published
---

# Parsing CSV with "NaN" values

Another question popped up today on `#haskell-beginners`. The usecase is pretty simple.
There is a csv file with the bytestring `NaN` inside, and you want to get these as `Double`.
The first thing is to actually get `Maybe Double`, `NaN` is evil, and it's better to be upfront
about invalid `Double`.

Let's see first how to solve this problem, and then, let's explore a couple of options to get GHC
"write" code for us. This is meant as an beginner/intermediate tutorial.

# Straightforward solution

## Csv and Types

Here's the csv we're going to be concerned about for this post. I ommited the header to simplify the
parsing. The first field is a tag, and the second field is a number.

```
coucou,42
boom,NaN
```


Here's the record we want to map row to:

```haskell
data MyRecord = MyRecord
  { description :: String
  , number :: Maybe Double
  }
  deriving (Show)
```

The expected result is to get `Nothing` when `NaN` is encountered.

## Parsing

[cassava](https://hackage.haskell.org/package/cassava) is the de-facto standard to parse csv. It provides
two typeclasses: [FromRecord](https://hackage.haskell.org/package/cassava-0.5.2.0/docs/Data-Csv.html#t:FromRecord) to parse a row into a record, and [FromField](https://hackage.haskell.org/package/cassava-0.5.2.0/docs/Data-Csv.html#t:FromField) to parse a given field from a raw `ByteString`.

So let's write these instances, by hand first.

```haskell
-- hailing from the cassava package
import qualified Data.Csv as Csv
import Data.Csv ((.!))

-- the venerable bytestring package
import qualified Data.ByteString as BS

instance Csv.FromRecord MyRecord where
  parseRecord rec
    | length rec == 2 = MyRecord <$> rec .! 0 <*> (rec .! 1 >>= parseMbNaN)
    | otherwise = fail "boom, not enough fields"

parseMbNaN :: BS.ByteString -> Csv.Parser (Maybe Double)
parseMbNaN raw =
  -- this is where OverloadedStrings comes into play, we can
  -- directly write "NaN" in code and we get a ByteString
  if raw == "NaN"
    then pure Nothing
    else Just <$> Csv.parseField raw
```

And then put everything together:

```haskell
-- from the equally venerable vector package
import qualified Data.Vector as V

main :: IO ()
main = do
  let raw = "coucou,42\nboom,NaN"
  let result :: Either String (V.Vector MyRecord)
      result = Csv.decodeWith Csv.defaultDecodeOptions Csv.NoHeader raw
  print result
```

# Automatic `FromRecord` instance with Generics

Cassava provides a way to automatically get a `FromRecord` instance if the datatype
derives `Generic`. If you're new to the `Generic` mechanism, the idea is that GHC can
produce an internal representation of data types, and it's possible to pattern match on
this structure to do plenty of useful things. [This tutorial](https://www.stackbuilders.com/tutorials/haskell/generics/) is very nice as an introduction to the concept.

I'm also going to throw in the `DerivingStrategies` language extension,
more for explanatory purpose than anything else. It allows the developper to specify
how to derive instances.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

import GHC.Generics (Generic)
-- ^ that's part of GHC, as the name suggest

data MyRecord = MyRecord
  { tag :: String
  , number :: Maybe Double
  }
  deriving stock (Show, Generic)
  --       ^            ^ thanks to DeriveGenerics
  --       | this is coming from DerivingStrategies

  deriving anyclass (Csv.FromRecord)
  -- ^ this is the magic line. We get the instance for free !
```

So that's an improvement, we don't need to write the `FromRecord` ourselves thanks
to cassava providing the machinery for that through Generics.

# Newtypes and `DerivingVia`

Now, the problem with the previous solution is that it doesn't work `/o\`.
The generically derived instance for `Maybe Double` returns `Nothing` if the field is empty,
but in this case, it's a `NaN` and so it will fails to parse. As an aside, don't forget to
write tests for these kind of situations ;)

So let's fix that with a newtype and a custom `FromField` instance so we can keep the automatic
`FromRecord` instance.

```haskell
data MyRecord = MyRecord
  { tag :: String
  , number :: MaybeNaN Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (Csv.FromRecord)

newtype MaybeNaN a = MaybeNaN { getMaybeNaN :: Maybe a }
  deriving (Show)

instance (Csv.FromField a) => Csv.FromField (MaybeNaN a) where
  parseField raw =
    if raw == "NaN"
      then pure (MaybeNaN Nothing)
      else MaybeNaN . Just <$> Csv.parseField raw
```

Now, everything works !

The name `MaybeNaN` isn't very nice though. Quite often you want a better name which
describes better the meaning of the field. This is where you can use `DerivingVia` extension
to get the behavior you want without having to write custom instance for each newtype.

```haskell
{-# LANGUAGE DerivingVia #-}
-- ^ this implies DerivingStrategies

newtype NullableNumber = NullableNumber { getNullableNumber :: Maybe Double }
  deriving stock (Show)
  deriving Csv.FromField via (MaybeNaN Double)
```

Or if you prefer, you can use a standalone deriving like so:
```haskell
{-# LANGUAGE StandaloneDeriving #-}

deriving via (MaybeNaN Double) instance (Csv.FromField NullableNumber)
```

# Conclusion

The Generic deriving mechanism is very powerfull and pretty widely used. Aeson is the other
very popular library exploiting this mechanism. It's an easy way to get a lot for free.
The most impressive usage of `Generic` I know is the library [generic-lenses](https://hackage.haskell.org/package/generic-lens) which gives you lenses and prism all thanks to `Generic`.
The `DerivingVia` mechanism is a nice addition to have concise yet extensible instances. It really
shines when you want the same instance on multiple different types.
