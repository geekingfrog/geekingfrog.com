---
title: Mocha and generators (2)
status: published
tags:
- ''
---

I've [blogged before](/mocha-and-generators/) about this topic and I had a problem when it comes to test exceptions.
The problem is caused by `Promise.spawn` which returns a promise and swallow all errors thrown inside. To still get a failing test if an unexpected error is thrown, the following form should be used:

```javascript
test('everything is good', function(done) {
  Promise.spawn(function* () {
    assert.equal(foo, 'true'); // foo is undefined
  }).catch(done); // ** note this line **
});
```

*Et voila*, this test will fail as expected.