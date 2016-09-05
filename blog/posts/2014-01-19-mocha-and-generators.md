---
title: Mocha and generators
tags: coroutine, mocha, test
status: published
---

Lately I have been writing a dependency injection module for node.js which works with coroutines. I'll probably write more about it later since I find this topic super interesting but for now, one can look at [this introduction](http://tobyho.com/2013/06/16/what-are-generators/) to generators. I am using the excellent [bluebird promises library](https://github.com/petkaantonov/bluebird) for the coroutine part.

So let's try to write a mocha test with coroutines, there are a few gotcha.

Here's our basic generator:

```javascript
function* generator() {
  yield 'ok';
  yield Promise.delay('late', 100);
}
```
(notice the * at the end). Here's how to use it *(node >= 0.11.4 with --harmony flag)*:

```javascript
var g = generator();
g.next(); // { value: ok, done: false }
g.next(); // { value: <promise object>, done: false }
g.next(); // { value: undefined, done: true }
```

A basic mocha test would be:

```javascript
suite('Async with generator', function() {
  test('basic generator', function() {
    var g = generator();
    assert.equal(g.next().value, 'ok');
    assert.equal(g.next().value, 'late');
  });
});
```

And this fails... Because the second call to `g.next()` returns the promise object. A fix would be:

```javascript
test('With synchronous yield', function(done) {
  var g = generator();
  assert.equal(g.next().value, 'ok');
  g.next().value.then(function(val) {
    assert.equal(val, 'late');
    done();
  });
});
```

This pass, but is not very pretty, and suddendly, we have to get back the asynchronous callback back :(

A better solution is to use a [coroutine](https://github.com/petkaantonov/bluebird/blob/master/API.md#promisecoroutinegeneratorfunction-generatorfunction---function).

```javascript
var Promise = require('bluebird');
test('With synchronous yield', Promise.coroutine(function* () {
  var g = generator();
  assert.equal(g.next().value, 'ok');
  assert.equal(yield g.next().value, 'late');
}));
```

Much nicer !

Be careful to one gotcha though, everything running inside the coroutine is actually wrapped inside a promise. So if there is any exception inside, it won't by default be thrown outside. For example:

```javascript
test('Promise rejected', Promise.coroutine(function* () {
  throw new Error('boom');
  assert.ok(true);
}));
```

This test will pass. You can add logging with

```javascript
Promise.onPossiblyUnhandledRejection(function(error){
  console.log('unhandled rejection detected !');
  console.log(error);
});
```

I haven't find yet a good enough solution to this problem, where I can have a failing test if an unexpected excepction is thrown.

If anyone has an idea ?