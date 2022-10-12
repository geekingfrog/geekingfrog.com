---
title: Haskell Exchange 2015
status: published
tags:
- haskell
- conference
---

Last Thursday and Friday was the Haskell Exchange 2015 in London. Even though I am a bit inexperienced in Haskell, these two days were a blast.
Below a few highlights:

## Into the core
The first talk of the conference. It was a general presentation of the intermediary language of GHC. It's a very small language (5 types and 15 data constructors), which is also strongly typed. Apparently in other languages, intermediate representations erase types.

## Ludwig DSL and Haskell goes devops
The takeaway from these two talks is that haskell is a fantastic language to glue other parts together. It can advantageously replace bash for orchestration, as it's a higher level language and a lend itself quite nicely to write DSL.

## High performance Haskell
Unpack lots data structure to avoid pointer overhead and cache miss with `{-# UNPACK #-}`. As always, measure for all such optimisation ([criterion](http://hackage.haskell.org/package/criterion)).

##Giving Haskell Types to a Relational Algebra
This was a presentation of how Standard Chattered introduced some kind of dependant type to their codebase. The idea is to have SQL statements statically typed, even when the type of the operation depend of the values (as `join` works for example). This talk was way over my head and *very* complex. Apparently dependant types are awesome, but introducing them in haskell is difficult.

##Fun with Haxl
A library used at facebook to automatically batch and cache requests to external sources. Very impressive and very powerful. It's used in the abuse prevention system at facebook and usually allow to make over 100 requests in 12 round trip, Haxl takes care of batching according to their relative dependencies.
I also discovered a new interesting pragma: `ApplicativeDo`, which automatically rewrite `do` notation as an applicative when possible.

##From type to Web Applications
An introduction to [servant](http://haskell-servant.github.io/), an awesome library to create API, with Type safe URLS and handlers, and the ability to automatically generate documentation (swagger style).

##Intro to opaleye
This was a workshop to get started with opaleye, a library to work with postgres. Although it was too quick to get the full picture, I will definitely considered using it if I have to use postgres.

## The javascript problem
This was an intro to GHCJS, a compiler from ghc to javascript. It works similarly to asmJS, where a runtime is embedded to then run specially crafted code. The supported range of feature is impressive (threads with STM, various haskell numeric datatype and so on). Outstanding work

##Park bench
This was an open discussion about haskell, the language and the ecosystem in general. One of the recurring point was about how to attract more people to the language (better doc and more tutorials and simple example), while keeping haskell a well designed language with strong academic influence.


Overall these two days were very inspiring and I'm already looking forward the 2016 edition.

