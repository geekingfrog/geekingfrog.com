---
title: Wishlist for a node API framework
status: published
tags:
- javascript
- thoughts
---

I have been working for 3 months now on a node.js server and so I started thinking more about how to build an API.
Below is a list, without any particular order, of things to consider. I'll probably end up writing some part of it with generators support and [koa](https://github.com/koajs/koa).

## Dependency injection
It's a pain to test some components in isolation because they require some complex stuff (database connection and mail client being good examples). So I definitely want a way to inject all dependencies to allow for easy mocking and unit testing.
See [this blog post](http://blog.plover.com/prog/Moonpig.html) for some interesting idea about mocking the time and about everything. DI will make really easy to perform unit test and mock stuff. Some possible candidates for a simple DI setup:

  * [Intravenous](https://github.com/RoyJacobs/intravenous) seems to be a pretty complete setup. Maybe overkill ?
  * [infect](https://github.com/amwmedia/infect.js) is a super tiny and simple DI stuff. (Too simple ?)
  * [dependable](https://github.com/idottv/dependable) is inspired by intravenous but is more simple. Need to study a little bit the doc first.

The caveat is that they don't support [ES6 generators](http://wiki.ecmascript.org/doku.php?id=harmony:generators). So I'm writing something simple to get around that.

## Access control
Need an authentication mechanism, and then, be able to have *nix like permission on api endpoint and/or document. Need to study Roles based access control for that. This can quickly become **very** tricky, so put some thought on that.

## JSON validation
Something like mongoose schema to control the input/output of the api. This lead to a lot of boilerplate code.

## Everything is JSON
All the api should be able to output JSON, and eventually plug these data into a rendering engine to output html. I very much like the idea of [Waigo](https://github.com/hiddentao/waigo) where `GET /user/userid/profile.json` force the output to json.

## DB agnostic
The framework should not impose any database or any interaction with it. Only hooks at some points. There are too many different DB and too many stuff. It's too complicated to create the abstraction for that (plus religious war).
I think the easiest way is to just not do anything for that and let the user implement its own service layer the way he wants.

## Use an advanced logger
Something like [Winston](https://github.com/flatiron/winston). Maybe try an optimistic logger like
[fingerCrossed](https://github.com/Seldaek/monolog/blob/master/src/Monolog/Handler/FingersCrossedHandler.php).
The idea is to only log stuff above a certain level untill an event at a higher level is encountered. From this point onwards, log everything.  

For example: log only INFO and above, until ERROR is seen. From then, log everything.

## Bonus: immutability
With [mori](https://github.com/swannodette/mori) coming from Clojure, it makes it possible to write functional javascript with immutable data structure. This might be impossible with koa though. Immutability is awesome, so it's worth a try.

I'm very motivated to try more functional javascript stuff and if I can find a way to write my small api server with it that would be great.

---
So that's about it. I think I'll see what I can do for a simple project and then either change framework, build my own (another one 囧) or contribute to some framework to add what I need.
