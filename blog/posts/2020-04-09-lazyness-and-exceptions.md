---
title: Lazyness and exceptions
tags: haskell
status: published
---

In haskell, evaluation is lazy, sometimes it's also reffered as "call by need". Historically, there were a lot of lazy languages and haskell was created to unify all of them. Nowaday, it's the only remaining language with lazy evaluation by default, at least to my knowledge.

It has some interesting properties, as it often makes functions more composable and reusable. For example, if you already have at hand a `sort` function, you don't need to modify it to only return the top 3 elements: `take 3 (sort myCollection)` works and doesn't sort the whole list.

But it also has some pitfals. Lazy IO is often frown upon, and in this post I'll quickly show why.

## Exceptions

In haskell, there are a few way to throw exceptions. The main one is from [Control.Exception](https://hackage.haskell.org/package/base/docs/Control-Exception.html): `throwIO :: Exception e => e -> IO a`. To handle exception, there are a couple of functions, the basic one is `try :: Exception e => IO a -> IO (Either e a)`. Notice the `IO` at the end. That means the only way to catch exceptions is to be in `IO`.

## Lazy evaluation

Let's examine the following program:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Exception as Exc

main :: IO ()
main = do
  result <- Exc.try $ pure (5 `div` 0)
  case result of
    Left (err :: Exc.SomeException) -> putStrLn $ "Caught error: " <> show err
    Right x -> putStrLn $ "Everything went well: " <> show x
```

If you run this, you'll get the following message `<program name>: divide by zero` and the exit code is 1.
So clearly, the `try` didn't do its job.  `SomeException` is the base case of exception, this should catch _any_ exception thrown.

The problem is with lazy evaluation. The problematic operation `div 5 0` isn't evaluated until it's actually needed, that is, until it is pattern matched against. And at that point, it's already out of the block covered by `try`.


## Forcing evaluation

In this simple example, we want to evaluate the expression sooner. There is the [evaluate](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#v:evaluate) function which does that (only to weak head normal form, details are out of scope for this post). The fix is:

```haskell
result <- Exc.try $ Exc.evaluate (5 `div` 0)
```

and this time, the program output `Caught error: divide by zero` and returns 0.

## Throwing exception in pure code

There is another way to manually throw exception: `throw :: Exception e => e -> a`. This one doesn't require to be in `IO`. Because of lazy evaluation, exactly _when_ this exception will be thrown is not obvious at all, and thus, it's near impossible to catch. Avoid `throw`, instead return an `Either`.

## Conclusion

Lazyness has subtle interplay with IO and exception. Performing IO action lazily makes handling exception much harder when it's possible at all.
The topic of exceptions in haskell is complex and subtle and this is only the tip of the iceberg. For more information, the [unliftIO](https://hackage.haskell.org/package/unliftio) package has some good reading. To perform IO in a streaming fashion, prefer a dedicated library like [conduit](http://github.com/snoyberg/conduit) instead of lazy IO.
