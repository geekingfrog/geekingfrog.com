---
title: Install erlang with mise
status: published
tags:
- TIL
---

To install erlang with [mise](https://mise.jdx.dev/dev-tools/) without warnings
about missing doc or odbc:

```bash
export KERL_CONFIGURE_OPTIONS="--without-javac --with-odbc=/var/lib/pacman/local/unixodbc-$(pacman -Q unixodbc | cut -d' ' -f2)"
export KERL_BUILD_DOCS=yes
mise install erlang
```
