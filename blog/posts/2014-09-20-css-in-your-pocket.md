---
title: CSS in your pocket
status: published
tags:
- performance
- css
- layout
---

Some notes from the talk: [Angelina Fabbro: CSS In Your Pocket - Mobile CSS Tips From The Trenches [CSSConfUS2014]](https://www.youtube.com/watch?v=vBHt61yDO9U):

# layout
Get the layout right **first**.

## Android 2.3 is the new IE6.
Below, a list to avoid if targetting Android 2.3 to avoid headaches.

* Avoid `position: fixed` and `backface-visibility: hidden`. The first is buggy in android 2.3 and doesn't behave well in ios. The second can have performances issues.
* `min-height/max-height, min-width/max-width` behaves inconstitently.
* Don't use `overflow: auto`.
* If using `z-index`, set `body { z-index: 0; }`.
* `ems` are poorly supported (don't even think about `rems`).
* no gradients

## IOS
* 'Magic' incantation for flexboxes:

```
.flex-container: {
  display: -webkit-box;  /* iOS6 */
  display: -moz-box;     /* old firefox */
  display: -ms-flexbox;  /* IE 10 */
  display: -webkit-flex; /* new chrome */
  display: flex;         /* new spec, opera 12.1, firefox 20+ */
}
```

# Performances
Stay away from:

* `*` selector
* `.lots .of .nested .classes`
* `border-radius` and `box-shadow` (both together destroys perf)
* `transform` (especially rotate)
* Aim for stable fps (a bit above 30)


Useful resources:

* [quirksmode.org](http://quirksmode.org)
* CSS coverage in firefox dev tools
* Use [Charles](http://charlesproxy.com) proxy to simulate slow connections.