---
title: Stream adventures
status: published
tags:
- node
- stream
---

Recently, I was trying to get some file from amazon S3 to compute their MD5. In the process, I found [this article](http://ejohn.org/blog/node-js-stream-playground/) from John Resig about steams in node (>=0.10). There was a link to a very fun tutorial about streams: [stream-adventure](https://github.com/substack/stream-adventure).

It's a pretty good introduction to streams, and I'm now much more comfortable with streams. My solutions can be found on [github](https://github.com/geekingfrog/stream-tutorial).

There is a bug on the last challenge though. The expected result is wrong and there is [a pull request](https://github.com/substack/stream-adventure/pull/21) to fix this, but no sign of life for 6 months.

Streams are super cool, it allow one to process a big input without actually storing everything in memory. It's also the same phylosophy of linux, where one can compose programs by piping them together. Ultimately, node stream can be like pieces of lego, where you put them together according to your need.