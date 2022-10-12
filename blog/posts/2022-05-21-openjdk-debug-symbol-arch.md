---
title: Debug symbols for openjdk8 on archlinux
status: draft
tags:
- linux
- java
---

* `asp export jdk8-openjdk` to get the package.
* edit `PKGBUILD` and edit the options line to add the following `debug !strip`.
* `makepkg -si` to build & install, takes around 20 minutes to compile on my machine.
This handle the integration with `archlinux-java`

Without symbols: `nm /usr/lib/jvm/java-8-openjdk/jre/lib/amd64/server/libjvm.so` shows `no symbols`.
with symbols, `nm /usr/lib/jvm/java-8-openjdk/jre/lib/amd64/server/libjvm.so`

https://archlinux.org/packages/extra/any/asp/
