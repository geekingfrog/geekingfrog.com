---
title: Some notes about Kriskowal's Q
tags: promise
status: published
---

I just finish reading the source code of <a href="https://github.com/kriskowal/q">Kriskowal's Q</a> library and below are some notes (mainly to my future self).
First in case someone stumble upon this post, this library is a complete implementation of the very cool pattern promise/defer for javascript and node.
<ul>
	<li>If performance is an issue, re-read the beginning about nextTick(). In the browser, setTimeout has a minimum value of 4ms, which *may* be problematic. There are workaround though.</li>
	<li>Need to start using uncurryThis. Basically it allow one to replace "".toUpperCase.call(s) by toUpperCase(s). Better for minimisation and also safer in case something modify Function.call.</li>
	<li>Hoisting make reading code difficult. Need to read the source twice to better understand.</li>
	<li>There are some functions (nfbind, synonym of denodify) which take a node-style function + callback and return a promise. Useful to refactor node code using promise/defer</li>
</ul>
There are tons of stuff I haven't understood, I'll need to re-read that a few times. And I was thinking I could implement this library myself pretty easily, still a long way to go.