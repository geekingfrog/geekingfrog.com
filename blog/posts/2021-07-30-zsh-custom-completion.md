---
title: ZSH custom completion
status: draft
tags:
- zsh
- shell
---

* `$words` is an array which holds the args and options currently being typed

Gotchas:

* when hacking on the same function, delete `$ZSH_COMPDUMP` to avoid stale cached function.

# Useful links

* customizing zsh completions with zstyle: [https://thevaluable.dev/zsh-completion-guide-examples/](https://thevaluable.dev/zsh-completion-guide-examples/).

* [zsh completion how-to](https://github.com/zsh-users/zsh-completions/blob/master/zsh-completions-howto.org).
* `man zshcompsys` for the man pages about the completion engine.
* [writing zsh completion scripts](https://blog.mads-hartmann.com/2017/08/06/writing-zsh-completion-scripts.html)
