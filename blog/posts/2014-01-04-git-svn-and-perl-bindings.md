---
title: git svn and perl bindings
tags: 
status: published
---

I'm starting a new project and this time I want to use git svn. I am tired of working with git in local but with svn with the repo and always switching between the two.

Except that I had a very annoying bug: <strong>Can’t locate SVN/Core.pm </strong>. I found numerous answer like "install libsvn-perl" but I already did that and still nothing worked (ubuntu 12.04).

So I took the long route: build subversion from source with perl bindings and then build git and git-svn.

For subversion: <a href="http://subversion.apache.org/download/">source</a> and <a href="http://www.linuxfromscratch.org/blfs/view/svn/general/subversion.html">instructions</a>. Make sure to read all the ouput of ./configure For example I had to install APR package. Also install swig for the perl bindings. Then the usual:
[bash]./configure --prefix=/usr --disable-static &amp;&amp; make
make swig-pl
sudo make install
sudo make install-swig-pl[/bash]
And here you go. After that I built git and git-svn from source (make/make install) and that was it.