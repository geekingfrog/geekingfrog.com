---
title: Be strict with your javascript
status: published
tags:
- javascript
- strict-mode
---

Today, my boss ask me help about a very weird bug:

```javascript
// foo comes from elsewhere, and he wants to use it
console.log(foo.stat); // 0
foo.stat = 42;
console.log(foo.stat); // 0
```

`foo` actually comes from a `webSQL` store and was not writable. With the addition of `'use strict';` at the beginning of the file, an exception was thrown and the confusion disapeared.

Conclusion: **always** put `'use strict';` at the beginning of your files to raise exception instead of failing silently.