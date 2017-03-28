---
title: Concurrent sources with conduit
tags: haskell, conduit
status: published
---

Recently I've been using quite a bit of [conduits](http://hackage.haskell.org/package/conduit). While building some tools to check for bugs in our dataset in dynamodb, I had to use the parallel scan feature. Since [amazonka](https://hackage.haskell.org/package/amazonka) (the haskell aws sdk) uses conduit when paging, I had to find a way to get multiple sources running concurrently and feeding into the next stage of the pipeline.

After spending an embarassingly long time figuring this out, I'll present a way to run multiple conduit source and collapse them into one single source.

# Dependencies and imports

```
conduit-combinators-1.1.1
async-2.1.0
stm-chans-3.0.0.4
stm-2.4.4.1
conduit-1.2.9
```

Most imports are qualified in case you're not familiar with the packages:

```haskell
import qualified Conduit as C
import Conduit ((.|))  -- because C..| is ugly
import Data.Conduit (bracketP) -- for setup and cleanup

-- we'll need the async package for cancellation
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async

-- queue utilities
import qualified Control.Concurrent.STM.TBMQueue as STM
import qualified Control.Concurrent.STM as STM

```

# Some setup

First, some helpers functions are required.

Turning a queue into a [source](http://hackage.haskell.org/package/conduit-1.2.9/docs/Data-Conduit.html#t:Source):
```haskell
sourceQueue :: C.MonadIO m => STM.TBMQueue o -> C.ConduitM i o m ()
sourceQueue queue = loop
  where
    loop = do
        mbItem <- C.liftIO $ STM.atomically (STM.readTBMQueue queue)
        case mbItem of
            Nothing -> pure ()  -- queue closed
            Just item -> C.yield item *> loop
```

And the reverse: feeding a queue from a conduit:

```haskell
sinkQueue queue = loop
  where
    loop = do
        mbItem <- C.await
        case mbItem of
            Nothing -> pure ()  -- no more items to come
            Just item -> do
                C.liftIO $ STM.atomically (STM.writeTBMQueue queue item)
                loop
```

Note that `sinkQueue` will not close the queue. Because multiple source will feed the same queue, we don't want the first source to terminate to close the queue and lose the inputs from the other sources.


# Parallel sources

The tricky bit here is to correctly handle exceptions (and cancellation). Thankfully, there is [bracketP](http://hackage.haskell.org/package/conduit-1.2.9/docs/Data-Conduit.html#v:bracketP).

First, the type signature:
```haskell
parSources :: (C.MonadIO m, C.MonadResource m) => [C.ConduitM () o IO ()] -> C.ConduitM () o m ()
parSources = error "work in progress"
```

Given a list of conduits, collapse all of them into one conduit, which will terminate when *all* sources are done.
The `MonadResource` constraint is a requirement of bracketP. This is what makes the initialisation and cleanup possible in a nicer way than a simple `bracket`.


```haskell
parSources :: (C.MonadIO m, C.MonadResource m) => [C.ConduitM () o IO ()] -> C.ConduitM () o m ()
parSources sources = bracketP init cleanup finalSource
  where
    init = do
        -- create the queue where all sources will put their items
        queue <- STM.newTBMQueueIO 100

        -- In a separate thread, run concurrently all conduits
        a <- Async.async $ do
            Async.mapConcurrently_ (\source -> C.runConduit (source .| sinkQueue queue)) sources
            -- once all conduits are done, close the queue
            STM.atomically (STM.closeTBMQueue queue)
        pure (a, queue)
    cleanup (async, queue) = do
        -- upon exception or cancellation, close the queue and cancel the threads
        STM.atomically (STM.closeTBMQueue queue)
        Async.cancel async
    finalSource (_, queue) = sourceQueue queue
```


# Putting it all together

First, a small helper to produce some items with a name and some delay between them

```haskell
namedSource :: (C.MonadIO m) => String -> C.ConduitM () String m ()
namedSource name = do
    C.yieldMany [1..5] .| C.mapMC delayItem .| C.mapC (\i -> name ++ " - " ++ show i)
  where
    delayItem x = C.liftIO (threadDelay 500000) *> pure x
```

And a way to print the items going through a conduit to make sure they are properly streamed

```haskell
logItem :: (Show a, C.MonadIO m) => a -> m a
logItem x = C.liftIO (print x) *> pure x
```

Finally, to run everything:

```haskell
main = do
    C.runResourceT $ C.runConduit $
        parSources [source1, source2] .| C.mapMC logItem .| C.sinkNull
    print "all done"
```

And that will output in a streaming fashion:

```
"source 2 - 1"
"source 1 - 1"
"source 2 - 2"
"source 1 - 2"
"source 2 - 3"
"source 1 - 3"
"source 1 - 4"
"source 2 - 4"
"source 2 - 5"
"source 1 - 5"
"source 1 - 6"
"source 2 - 6"
"source 1 - 7"
"source 2 - 7"
"source 1 - 8"
"source 2 - 8"
"source 2 - 9"
"source 1 - 9"
"source 1 - 10"
"source 2 - 10"
"all done"
```

# Conclusion
This can be used whenever lots of slow operations are done simultaneously and you want to process the results with the nice conduit abstraction. For example, getting 100 items from a parallel scan in dynamoDB, or the first 10 pages from a web crawl.

I'm still unsure how to get a more generic constraint for the sources:

```haskell
parSources :: (C.MonadIO m, C.MonadResource m) => [C.ConduitM () o IO ()] -> C.ConduitM () o m ()

-- ideally I'd like the sources to have a generic type not directly involving IO:
MonadIO m => [C.ConduitM () o m ()] ...
```
