---
title: Vim and ssh
status: published
tags:
- ssh
- vim
---

One more reason I love vim: I can ssh to a remote server, and still have my custom editor without any inconvenience.
Plus, I'm using [Vundle](https://github.com/gmarik/Vundle.vim) which manage all my plugins from my [.vimrc](https://github.com/geekingfrog/configFiles/blob/master/.vimrc). So even on a brand new server, I can get my beloved editor working in 2 minutes.

This is really bad practice to modify a running server live, but when developping a new service, on a test server, ssh+vim is a great combo.