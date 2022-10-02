---
title: Advanced objects in javascript
status: published
tags:
- javascript
---

On hacker news: [this article](http://bjorn.tipling.com/advanced-objects-in-javascript) describing all the advanced features of objects in javascript (like defineProperties, getter/setter, seal, freeze, proxies and much more). Really good article.

One immediate takeaway for me: prefer using `Function.prototype.isPrototypeOf` to `instanceOf` since the later does not work with objects created with `Object.create`.