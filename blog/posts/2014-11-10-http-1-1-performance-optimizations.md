---
title: HTTP 1.1 performance optimizations
tags: web, performance
status: published
---

So recently I changed [my website](//geekingfrog.com), and I wanted to optimize everything I could, mostly for fun. I've [read](http://chimera.labs.oreilly.com/books/1230000000545/index.html) and [watched](https://www.youtube.com/watch?v=FEs2jgZBaQA) quite a lot of stuff about it, and it was a good way to start practicing.

## Measuring
The first step when optimizing anything is to measure. For this, I'll be using [page speed insights](https://developers.google.com/speed/pagespeed/insights/) from Google and [web page test](http://www.webpagetest.org). I'll also focus on mobile, that's where one gets more bangs for their bucks. And a really fast mobile website will (almost?) always translate to a fast desktop website.

## Step 0
First version of the website online, with absolutely no consideration about speed.
![](/static/images/2014/11/pagespeedDesktop00.png)

Mobile:
![](/static/images/2014/11/pagespeedMobile00.png)
![](/static/images/2014/11/pagespeedMobile00-1.png)


I got a terrible score for user experience because there wasn't even a proper viewport setup.
**The fix**
```html
<meta name="viewport" content="width=device-width" initial-scale="1">
```
And make my site responsive also. The result:
![](/static/images/2014/11/pagespeedMobile01.png)
Much better.

Now let's improve the speed.

## Minifying & compressing
After that, I added a minification steps:

* Using [jade](http://jade-lang.com/) templates, the output is already minified html.
* I'm using [gulp](http://gulpjs.com/) as a build tool. The plugin [gulp-minify-css](https://www.npmjs.org/package/gulp-minify-css) took care of minification
* No scripts (yet)

I also enabled `gzip` compression. This is supported by **all** browsers (including linx, curl, and down to IE5). There is just no reason not to use it.

## Optimizing images
I used [gulp-imagemin](https://www.npmjs.org/package/gulp-imagemin) to optimize the few images I'm using. It's really easy to setup and can save off quite a few kB.

## Caching
In the same step, I also added some caching mechanisms:

* Added an [Etag header](https://developers.google.com/web/fundamentals/performance/optimizing-content-efficiency/http-caching#validating-cached-responses-with-etags). This way, if an asset hasn't changed between two requests, only the headers will be transmitted, with a response of `304`, and no body. There is nothing faster than a non transmitted bit.
* Leverage http caching. By adding the [proper header](https://developers.google.com/web/fundamentals/performance/optimizing-content-efficiency/http-caching#cache-control):
```
Cache-Control: "max-age=432000"
```
This number is in seconds, and means the browser can re-use the response for the next five days.

**Result**
![](/static/images/2014/11/pagespeedMobile02.png)
We're getting closer. It's already very good, and it would be enough in most cases, but I wanted to go even further.

## Eliminate render-blocking css
The last step is to inline the css for above-the-fold content. This way, the browser doesn't have to wait to receive the css file before displaying something.
For this task, I used a tool called [critical](https://github.com/addyosmani/critical). It takes as an input an html page and a viewport size, and output the css rules used to render the page within the viewport. Internally, it uses phantomJS, so it also works with injected scripts and stylesheets. Although it's experimental, it works very well.

In my case, I had a bit of extra work to do because I was using jade, so I didn't have a ready to use html file, and I couldn't directly inline the critical css (the tool can also directly inline critical css).

Anyway, after this last step, here's the result:
![](/static/images/2014/11/pagespeedMobile03.png)

Yeah!

## 98 !== 100
PageSpeed insights keeps bugging me about how I do not leverage browser cache:
![](/static/images/2014/11/pagespeedMobile03-1.png)
Which contradicts what I see using the devtools of firefox or chromium.
I'll keep digging a bit but I'm satisfied for the moment.

## What's next?
Next, I'll have some fun optimizing TLS and nginx settings so I can establish the http**s** connection in one RTT (that's the goal at least).