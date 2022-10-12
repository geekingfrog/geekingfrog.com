---
title: '`for in` and prototypes'
status: published
tags:
- javascr
- prototype
---

# The problem
Sometimes, I see code like:

```javascript
var s = 'some string';
for(var i in s) {
  //...
}
```
or

```javascript
var a = ['foo', 'bar'];
for(var i in a) {
  //...
}
```

I had a bad feeling about this until I got bitten by it recently. Consider the following code:

```javascript
String.prototype.baz = 'baz';
var s = 'foo';
for(var i in s) {
  console.log(s[i]);
}
// f
// o
// o
// baz
```

#Best practices
So `for in` is convenient but one have to remember that it also iterates over the **prototype** of the object.
If you want to iterate over a string, prefer something like:
```javascript
for(var i=0,n=s.length; i<n; ++i) {
  // s[i] will only goes over the characters
}
```

For an array, you can even use `forEach`
```javascript
myArray.forEach(function(val) {
  // it will conveniently skip holes in the array
});
```

So when should you use `for in`? When you want to iterate over the keys of an objects. The following two ways are corrects:

```javascript
for(var i in o) {
  if(!o.hasOwnProperty(i)) continue;
  // do something here
}

// or
Object.keys(o).forEach(function(key) {
  // do something with o[key]
})
```
`lodash` has also a method to achieve the same effect: [`_.forOwn`](http://lodash.com/docs#forOwn).

So remember that `for in` will goes through the prototypes of your object as well. Since extending the prototype of built-in objects is questionable (but it's a bit religious here), and some libraries do that, prefer robust code.