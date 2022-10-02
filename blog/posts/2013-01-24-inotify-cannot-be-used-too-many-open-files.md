---
title: inotify cannot be used; too many open files
status: published
tags:
- linux
---

Today, while I was doing a little bit of dev on a server, I got with a tail:
[bash]
tail: inotify cannot be used, reverting to polling: Too many open files
[/bash]

Below, a quick command to get the culprits:
`lsof | awk '{ print $2; }' | uniq -c | sort -rn | head`

The first columns is the number of open file, and the second column is the pid