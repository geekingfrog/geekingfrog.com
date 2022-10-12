---
title: Crash Course on Web Performance
status: published
tags:
- web
- performance
---

I just finished a <a href="http://www.igvita.com/2013/01/15/faster-websites-crash-course-on-web-performance/">very interesting talk</a> by <a title="Ilya Grigorik" href="http://www.igvita.com/">Ilya Grigorik</a>, a Google engineer about web performance. It covers a lot of different aspect, including tcp latency, dns resolution, browser internals to fetch and parse the pages end so on.

One quick note about a cool trick when using google analytics:

```
_gaq.push(['_setSiteSpeedSampleRate', 100]);
```

This tell the analytics tool to sample 100% of your users. The limit is 10k visits per day, so for personal websites it's definitely not a problem.