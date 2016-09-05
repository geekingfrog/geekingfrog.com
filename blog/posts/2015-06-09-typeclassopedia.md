---
title: Typeclassopedia
tags: haskell
status: draft
---

I've been going through the very good [typeclassopedia](https://wiki.haskell.org/Typeclassopedia) to better understand haskell types and their relations with each others.

Below are my solutions to the exercises.

##Functors

##Applicatives

##Monads
```
-- The free monad
data Free f a = Var a | Node (f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap ff (Var a) = Var (ff a)
  fmap ff (Node x) = Node $ fmap (fmap ff) x

instance (Functor f) => Applicative (Free f) where
  pure = Var
  (Var ff) <*> x = fmap ff x
  (Node ff) <*> x = Node $ fmap (<*> x) ff

instance (Functor f) => Monad (Free f) where
  return = pure
  (Var a) >>= ff = ff a
  m >>= f = join $ fmap f m
```

```
-- monad composition providing we have the distrib operation
distrib :: (Monad m, Monad n) => n (m a) -> m (n a)

join'' :: (Functor m, Functor n, Monad m, Monad n) => m (n (m a)) -> n (m a)
join'' = fmap join . distrib

join' :: (Functor m, Functor n, Monad m, Monad n) => m (n (m (n a))) -> m (n a)
join' = fmap join . distrib . join''
```

Although all monads are also functors, had to add the class constraints `Functor m, Functor n` since I'm using `ghc < 7.10`. Another workaround would be to use `liftM` instead of `fmap / <$>`.
