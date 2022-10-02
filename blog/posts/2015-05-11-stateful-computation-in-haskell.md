---
title: Stateful computation in Haskell
status: published
tags:
- monad
- state-monad
---

Now that I'm comfortable with the simpler monads like `Maybe` and `List`, I challenged myself to redo a small [coding challenge](http://challenge.shopcurbside.com/) I saw a few month ago using haskell and the `State` monad.

## Constraints and objectives
The challenge boils down to make authenticated requests to an http api and collect the data which form a tree. The leaves of the tree ultimately form a string. There are a few tricks there, but the main one is that the auth token is valid *only for 10 requests*. This is an excellent exercice to practice the following:

* Make HTTP requests (with custom headers)
* Parse JSON response
* Keep track of a state between requests (the token)

## Getting Started
For the cabal file:
```
  build-depends:       base >=4.8 && <4.9,
                       bytestring,
                       text,
                       vector,
                       mtl >= 2.2.1 && <3,
                       containers >= 0.5.6.3,
                       lens >= 4.5,
                       aeson >= 0.7.0.3,
                       lens-aeson,
                       wreq-sb >= 0.4 && < 0.5,
                       transformers

```
I went for [http://www.serpentine.com/wreq/tutorial.html](wreq) to make http requests. It's a simple and complete library. The other important dependency is the `mtl` to get the `State` monad.

## The state monad
The star of the post. In Haskell, functions are *pure* which means they don't have side effects and their output is only a function of their input (no access to outside state/variables).
Of course, sometimes, carrying a state between computation is required. Although this information could be embedded as an additional parameter, it quickly become cumbersome, and it makes composition of functions more difficult.
As usual in Haskell, monads provide an elegant solution to this problem.

The state monad is usually introduced as:
```
newtype State s a = State { runState :: s -> (a, s) }
```
This was **very** confusing for me. The key is to see that it's just a wrapper around a function of type `s -> (a, s)`. The next difficulty is how to construct such object. There are two methods. One which directly uses a constructor:
```
initialState :: State String a
initialState = state (\s -> compute s) -- or state compute
```
Another method is to use the `do` notation:

```
initialState :: State String a
initialState = do
  state <- get
  let (result, nextState) = compute state
  put nextState
  return result
```
`get` is provided by the State monad, and allow one to retrieve the internal state. `compute` here is just a pure function (which needs to have type `String -> (a, String)`). `put` set the internal state and `return` set the result of the computation.

From this monad, to extract the result, one has to use `runState`:
```
λ: :type runState
runState :: State s a -> s -> (a, s)
```
So for example:
```
let (result, nextState) = runState initialState "foo"
```

## Dealing with impure functions
In my case, the function to compute the next state involve making an http request, and this is impure. That's where `monad transformers` come into play. They allow one monad to be executed in the context of another monad. In my case, I had to use `StateT s IO a` which means a function with the signature `s -> IO (a, s)`. This special monad can transform its state.

## The code
First, some type aliases:
```
type Id = String
type Result = String
type Session = (Int, BI.ByteString)
type CrawlState = ([Id], Session)
```

The core function embedded into the state monad:
```
queryApi :: ([Id], Session) -> IO (Maybe Result, ([Id], Session))
queryApi ([], session) = return (Nothing, ([], session))
queryApi (currentId:nextIds, session) = do
  (body, newSess) <- liftIO $ fetchNext currentId session
  let nextTargets = extractIds body
  let res = extractResult body
  return (res, (nextTargets ++ nextIds, newSess))
```
Note the `liftIO` here, which means the given action has to be executed in the IO context. This is valid here because the transformer is `IO`.

The next step is to use this function into a State monad and pass it around until I got all the info from the api. This is the `gather` function:

```
gather :: StateT CrawlState IO (Maybe Result) -> CrawlState
  -> IO ([Maybe Result], CrawlState)
gather mState s = do
  (res, newState@(ids, _)) <- runStateT mState s
  case ids of
    [] -> return ([res], newState)
    _ -> do
      (nextResults, nextState) <- gather mState newState
      return (res : nextResults, nextState)
```
`runStateT` is the equivalent of `runState` and allow to retrieve the result and the new state from the compution embedded inside the monad. Then, depending on the new state (is there more stuff to fetch?), I'm either done or I recurse with the new updated state.


The `main` function is very simple:
```
main :: IO ()
main = do
  initialSession <- newSession
  let initialState = (["start"], initialSession)
  (results, _) <- gather (S.StateT queryApi) initialState
  putStrLn $ show $ concat . catMaybes $ results
```

The full code is [on github](https://github.com/geekingfrog/learnHaskell/tree/master/stateMonad).

## Conclusion
This was an interesting exercise, although I'm pretty sure it would be easier to just pass the state as function argument here. This was also a neat opportunity to do network requests, as a web dev, it's my bread and butter and it feels more practical than most abstract exercises from books I've read.

[This post](http://blog.jle.im/entry/unique-sample-drawing-searches-with-list-and-statet) also uses the `StateT` monad (combined with the list monad) in a very clever way, and is well worth the read.