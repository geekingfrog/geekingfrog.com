---
title: My frontend toolchain
status: published
tags:
- javascript
- tool
---

I'm a bit of a tooling nerd, I believe having good tool is crucial to do a good job and be efficient. I've recently played around quite a bit of new technologies and I'm now very happy with my front-end toolchain.

## Modern tools and libraries

* [Browserify](http://browserify.org/) to use the awesomness of commonJS and npm. Huge ecosystem and good module system. The first time it seems weird, but it actually just work.
* [Jest](http://facebook.github.io/jest/) from facebook. It's a testing library built on top of jasmine, which **automatically mock dependencies**. This is incredibly awesome and cannot be over-emphazied. It makes testing modules so much easier!
* ES6 with [6to5](https://6to5.org/index.html) and [es6-shim](https://www.npmjs.com/package/es6-shim). The es6-shim is optional if you only want to add the syntaxic sugar. I believe however that having [destructuring assignment](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment), [default parameters](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Default_parameters) and [rest parameter](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/rest_parameters) make the code much more readable. One can see how to use a function with just the first line. No need to scan through the first lines to see if there any parameter handling (like `if(!foo) foo='foo';`)
* [React.js](https://facebook.github.io/react/) with [flux](https://github.com/facebook/flux) as the general architecture. React encourage the creation of small and reusable components (don't forget to use `propTypes`). Flux makes testing and dependency management easy. It's a bit weird when starting but it scales very well and keeps the original simplicity even when the app grows in complexity.
* [gulp](http://gulpjs.com/) as the build tool. Using [watchify](https://github.com/gulpjs/gulp/blob/master/docs/recipes/fast-browserify-builds-with-watchify.md) to build the browserified bundle faster (I want a feedback loop below 1s).

## Bleeding edges experiments
I also tested a few things but I believe they are not ready for prime time **yet**.

* [Immutable.js](https://github.com/facebook/immutable-js) to get lazy and immutable collections in javascript. This can be very handy and reduce bugs due to different modules modifying shared data. The api is quite complex though and I'm trying to keep the api surface of my apps as small as possible (influenced from [this talk of jsconf EU 2014](https://www.youtube.com/watch?v=4anAwXYqLG8)).
* [flow](http://flowtype.org/) which allow to **gradually** add type informations in your javascript. This is an incredible tool but there are too many rough edges for the moment. The integration with 3rd-party libraries makes it impractical for the moment. The team is planning to add support for `.d.ts` files though so it'll soon be quite good.

This is now my default tool chain when starting a new front-end client. Usually I add the css files through bower. It's a very strong base, easy to start with and scales well too.