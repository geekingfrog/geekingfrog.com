---
title: Cache-Control & PageSpeed insights
tags: web, performance
status: published
---

In the [last post](http://blog.geekingfrog.com/http-1-1-performance-optimizations/) I ended up with a PageSpeed insight score of 98/100, and I was wondering why.

Apparently, PageSpeed insight doesn't like the header `Cache-Control: max-age=xxx`. I removed it and:
![](/static/images/2014/11/pagespeedMobile04.png)

\o/

Except that's stupid since now, the browsers won't ever cache my page and assets and will get 304 most of the time. So once I got this screenshot, I re-enabled the `Cache-Control` header, it makes more sense.