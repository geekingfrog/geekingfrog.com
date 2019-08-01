---
title: Custom glibc version for haskell binaries
tags: haskell, libc
status: published
---

# The problem

I have an IRC bot that I build on my local machine, and I want to run it on my server.
I'm running arch linux at home, which has the latest glibc (2.29 at the time of writing), but
my server running debian ships with 2.28.
I used to build the binary inside a docker container with the same versions as my server, but I'm currently on
holiday, with a laptop with barely enough disk space for that.

# Statically linked binary − a dead end
So at first, I tried to build a static binary that I could run on my server. To do so, I added a couple of things
to the `cabal` file:

```
cc-options: -static
ld-options: -static -pthread
ghc-options: -optl-static -optl-pthread -static
```

Then rebuild the whole thing `stack build --force-dirty`, copy the binary somewhere convenient: `stack install --local-bin-path .`
and check it's really statically linked: `ldd exec-name`. You should get something like:

```
ldd exec-name
  not a dynamic executable
```

But during compilation, I got a lot of warning like:
```
warning: Using 'endservent' in statically linked applications requires at runtime the shared libraries from the glibc version used for linking
```

I could run the application just fine on my server with the static binary, until I tried to make an http request, and then, it crashed with a `segfault`. /o\\


# Dynamically linked binary with custom glibc

So I ended up with this solution, which requires to build the correct version of glibc on the server. Follow [the instructions](https://sourceware.org/glibc/wiki/Testing/Builds)
to build the library. Don't forget to run `make install` at the end. This will create a directory `dist/lib` (or just `lib`) in the folder you ran `make build`. Let's say
the library end up at `/home/lambda/glibc-build/lib/`.

Then, following [this stackoverflow answer](https://stackoverflow.com/a/8658468), you need to specify where to find the required libraries when starting the executable
```
/home/lambda/glibc-build/lib/ld-2.29.so \
  --library-path "/home/lambda/glibc-build/lib:/lib/x86_64-linux-gnu:/usr/lib/x86_64-linux-gnu/" \
  /home/lambda/executable
```

Besides `libc`, a haskell application needs a couple of other shared object, like `libz.so` and `libgmp.so`. These should be on your system already.

And that's it. Finally I can run this crucial IRC bot without having to muck around with docker.
