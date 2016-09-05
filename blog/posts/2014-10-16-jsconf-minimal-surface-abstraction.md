---
title: JSConf: Minimal surface abstraction
tags: javascript, jsconf
status: published
---

[Great talk](https://www.youtube.com/watch?v=4anAwXYqLG8) given at JSConf EU 2014 about javascripts patterns and API surface.

* Use standard instead of javascript: it's ubiquitous.
* Patterns are also ubiquitous, no call to black box library.
* No abstraction is better than the wrong abstraction. It's much easier to recover. Which means (and it's very counter intuitive) that spaghetti code *can* be better than structured code.
* Find repetition and fix it *only* if it causes bugs. Make the abstraction worth its weight.
* When creating an abstraction, remove as much API surface as much as you add.
* Is easier to upgrade an *explicit* api with sugar syntax than to recover from an *implicit* API.

Conclusion: put JS on a diet:

* Follow slow moving standards. Use polyfills instead of frameworks or libraries.
* Prefer **explicit** repetitive code.
* Only abstract to solve bug.
* Burn as much as you add. Rethink the stack and purge.