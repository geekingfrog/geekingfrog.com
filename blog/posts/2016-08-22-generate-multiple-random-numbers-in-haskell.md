---
title: Generate multiple random numbers in haskell
status: published
tags:
- haskell
- monad
- state-monad
---

# 1) System.Random.randoms
[Simplest way](https://hackage.haskell.org/package/random-1.0.1.1/docs/System-Random.html) to generate an infinite list of random numbers.

```haskell
import System.Random as R

main = do
  gen <- R.getStdGen
  print $ take 10 $ R.randoms gen
```

Internally, `randoms` construct an infinite list:

```
randoms g = (\(x,g') -> x : randoms g') (random g)
```


# 2) State monad
The previous method is simple, but doesn't return a generator, so effectively, the generated numbers are the only one available. To get some random variable **and** a new generator, the state monad can be used.

```haskell
import Control.Monad.State

genRng :: RandomGen g => State g Int
genRng = do
  gen <- get
  let (val, gen') = R.random gen
  put gen'
  return val

main = do
  gen <- R.getStdGen
  -- get 10 random numbers and the final generator
  let (vals, finalGen) = runState (replicateM 10 genRng) gen
  print vals
```

This approach also lends itself to more complex operation, for example, returning a tuple:

```haskell
genRngTuple = do
  gen <- get
  let (val1, gen') = R.random gen
  let (val2, gen'') = R.random gen'
  put gen''
  return (val1, val2)
```


# 3) Simplifing with `state`
The `genRng` function is a bit verbose and can be simplified with [`state`](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Class.html#v:state)

```haskell
import Control.Monad.State

genRng' :: RandomGen g => State g Int
genRng' = state R.Random

main = do
  gen <- R.getStdGen
  print $ evalState (replicateM 10 genRng') gen
```

This approach is much nicer when returning tuple for example:

```haskell
genRngTuple' = do
  a <- state R.random
  b <- state R.random
  return (a, b)
```

This way, no need to do any bookkeeping with the internal state.

----

Here are three ways to generate multiple random values. These method works for any kind of stateful computation. The monad approach gives more control, allowing any function like `s -> (a, s)` to be embedded inside, abstracting away any work to thread the state across calls.
The State monad can also be generalized using monad transformer. Some good read about the transformer approach: 

* [Send more money](https://blog.jle.im/entry/unique-sample-drawing-searches-with-list-and-statet.html)
* and the follow up: [Sending even more money](https://www.schoolofhaskell.com/user/chowells79/even-more-money).
