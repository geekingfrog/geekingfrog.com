---
title: Akka camel actor and preStart method
status: published
tags:
- java
- akka
- camel
---

A quick notice if someone has trouble with camel UntypedConsumerActor. One <strong>must</strong> call super.preStart() if preStart method is overriden, otherwise the endpoint will not be published. I haven't seen that in the doc, maybe an omission or just me who didn't spot the info.

For those who don't know what <a href="http://akka.io/">akka</a> is, it's pretty cool. Basically it's a framework written in scala (with also a java api) to allow "easy" writting of concurrent application. It is based on the actor model, where objects are closed entities which share information using message passing. This remove the probleme of locking resources :)