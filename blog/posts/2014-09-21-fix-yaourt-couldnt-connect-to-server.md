---
title: Fix yaourt `couldn't connect to server`
status: published
tags:
- linux
---

I fixed it by hardcoding the ip address of `aur.archlinux.org` in `/etc/hosts`. This is by no mean ideal, and will break as soon as the server changes its IP, but it works.