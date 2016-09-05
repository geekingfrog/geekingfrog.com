---
title: Fix packages for commands
tags: java, linux, firefoxos
status: draft
---

While building firefoxOS on docker (from ubuntu 14.04 base image), at some point I needed to install java6. [The instructions](http://ubuntuhandbook.org/index.php/2014/02/install-oracle-java-6-7-or-8-ubuntu-14-04/) are fairly straightforward, except I got:

```
sudo: add-apt-repository: command not found
```

The fix [found here](http://ostechnix.wordpress.com/2013/04/29/resolve-the-error-add-apt-repository-command-not-found-in-ubuntu-12-10/) is to install the package `software-properties-common`, and that fixed it \o/.