---
title: 'svn: No repository found'
status: published
tags:
- svn
---

This title is what I encountered today while I was trying to access my own svn server. After a while I figured out the problem was quite simple. Last time I set up svn, I created a wrapper script to start <em>svnserve.</em> The goal was to allow people to connect to my svn while not knowing the full path (and deny them the access using full path). I guess a system update rewrited the file, so no more "short url" to connect to the server, only full path.

I did again what is written <a href="http://svn.haxx.se/dev/archive-2004-03/0253.shtml">there</a> and it's fine again.