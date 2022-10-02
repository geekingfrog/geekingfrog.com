---
title: Polling with promises
status: published
tags:
- promise
- javascript
---

I'm a big fan of promises in javascript. I think they are a cleaner way to express asynchronous operations than callback. They encourage re-use, are immutable and you don't care *when* your data is ready. Unfortunately, they are not well understood by the community, and few people are using it.

Here I want to show a quick example to do polling with promises. It came up at work a few days ago. We have to convert video to a given format. To do so, we're using a [thirs party](http://zencoder.com) which does that. We submit it a job, and then we need to poll its api to see when the job is completed.

Here's the callback based implementation:

```javascript
function poll(jobId, retry, cb) {
  if('function' === typeof retry) {
    cb = retry;
    retry = 5;
  }
  
  if(!retry--) {
    return cb(new Error('Too many retries');
  }
  
  api.getJobStatus(jobId, function(err, data) {
    if(err) return cb(err);
    if(data.state === 'finished') return cb(null, data);
    if(data.state === 'error') return cb(data.error);
    setTimeout(function() {
      poll(jobId, retry, cb);
    }, 10000); // retry in 10s
  });
}
```

Below is the variant using promises. I am using [bluebird](https://github.com/petkaantonov/bluebird), a fantastic library for promises, which has a method to transform callback based apis into promise based.

```javascript
var getJobStatusAsync = Promise.promisifyAll(api);

function poll(jobId, retry) {
  if(!retry) retry = 5;
  if(!retry--) throw new Error('Too many retries');
  
  return getJobStatusAsync(jobId)
  .then(function(data) {
    if(data.state === 'error') throw new Error(data.error);
    if(data.state === 'finished') return data;
    return Promise.delay(jobId, 10000).then(poll);
  });
```

And that's it, `Promise.delay` really helps to keep things clean.


