---
title: Reuse javascript regexp, global flag gotcha
tags: javascript
status: published
---

I'm working on a small pre-interview assignment for Mozilla (more in a future post), and I stumbled on a very weird bug.

The goal is to create an autocomplete widget for a text input. To filter the data based on the user input, I used a regular expression, and here is what I got:

```
var data = ['foo1', 'foo2', 'foo3', 'foo4'];
var text = 'foo';
var regexp = new RegExp(text, 'gi');
var filtered = data.filter(function(datum) {
  return regex.test(datum);
}); // filtered = ['foo1', 'foo3'] !!!
```

So basically, reusing the regexp causes it to return alternatively true and false.

This is due to the use of the 'g' flag, which tells the regexp to keep the index of the last match. So to make this example working, one has to manually reset the index. The filter loop becomes:

```
var filtered = data.filter(function(datum) {
  regexp.lastIndex = 0;
  return regexp.test(datum);
});
```

As for me, I dropped the 'g' flag which is not very useful in my case.