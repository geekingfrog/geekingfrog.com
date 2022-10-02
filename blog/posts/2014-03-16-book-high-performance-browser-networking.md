---
title: 'Book: High performance browser networking'
status: published
tags:
- book
- performance
- network
---

I recently finished [this excellent book](http://chimera.labs.oreilly.com/books/1230000000545) written by Ilya Grigorik, an engineer at Google. This book is really really good and if you are even remotely interested in web performances you should definitely read it.

It's very dense, packed with information and practical tips. I'm considering doing one or two quick presentation at my [local meetup](http://www.meetup.com/javascript-enthusiasts/). It's focused on network performances, which are the most important (css and rendering performances are the icing on the cake, do it only when you have a fast loading website).

A few examples:

* Why latency matters more than bandwidth.
* Why you should bundle and minify all your scripts and css to minimize the number of requests (inner working of TCP).
* How you can tune your kernel, TLS configuration and server settings to [save one RTT](http://www.igvita.com/2013/12/16/optimizing-nginx-tls-time-to-first-byte/)
* Why you should use browser caching, and understand the tradeof of sharding resources.
* Inner working of http2 and why it'll be awesome
* How the "futures" API work (websocket, server sent events and webRTC)
* And so more...

I highly recommend this book one more time for any web engineer (front end, back end or both). Even though we tend to treat the browser as a black box, there is a point where we need to understand its inner working to deliver better quality products.