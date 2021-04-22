---
title: Cross compiling rust
tags: rust, raspberry
status: published
---

# Cross compiling from linux to a raspberry pi

So I just got [this starter kit](https://camjam.me/?page_id=236): a small breadboard with a few simple components like LED, button and buzzer because I wanted to experiment a bit with [GPIO](https://en.wikipedia.org/wiki/General-purpose_input/output).
I opted out of [easy mode](https://pypi.org/project/gpiozero/) and decided to do that in rust. That seems like a good fit, the language is low level and allow full control of the underlying hardware.

But before GPIO, I needed a Hello World.


## Find the right target
[Rustup](https://rustup.rs/) has the concept of target and toolchains. So in theory, I can simply `cargo build --target ???` and off I go.

A target in this context is the [triple](https://wiki.osdev.org/Target_Triplet) `machine-vendor-operatingsystem`
. Thankfully, I have gcc installed on my raspberry pi: `gcc -dumpmachine` gives me `arm-linux-gnueabihf`.
Now off to the list of [supported targets](https://doc.rust-lang.org/nightly/rustc/platform-support.html) for rustc! And I found no exact match.

There are two targets looking promising though:
```
arm-unknown-linux-gnueabihf
armv7-unknown-linux-gnueabihf
```

`uname -m` gives me `armv7l`, so I guess I'll roll with `armv7-unknown-linux-gnueabihf`


## Linker woes

Install the toolchain: `rustup target add armv7-unknown-linux-gnueabihf` and then:

`cargo build --release --target armv7-unknown-linux-gnueabihf` vomits a scary looking error ending with:

```
/usr/bin/ld: rpi/target/armv7-unknown-linux-gnueabihf/release/deps/rpi-53051a58c3d5ca6d.rpi.9tay4t3s-cgu.0.rcgu.o: error adding symbols: file in wrong format
collect2: error: ld returned 1 exit status
```

😱 😭

Not that straightforward. Thankfully that's when I stumbled upon this [fantastic post](https://chacin.dev/blog/cross-compiling-rust-for-the-raspberry-pi/) which explains how to properly setup the toolchain.

Turn out, I need a whole set of binaries working for arm. In case the other post goes dark, I'll repeat the steps.
Head over to the [arm website](https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain/gnu-a/downloads) and download the correct archive. In my case, since I'm compiling from a linux host, I chose `gcc-arm-10.2-2020.11-mingw-w64-i686-arm-none-linux-gnueabihf.tar.xz`.

Unpack, and add the `bin` directory from that archive to your path: `export PATH="$(pwd)/gcc-arm-10.2-2020.11-x86_64-arm-none-linux-gnueabihf/bin:$PATH"`.

However this is not enough yet, I need to tell cargo to use this specific arm linker. According to the [section about configuration](https://doc.rust-lang.org/cargo/reference/config.html) in the cargo book, I can do this through the file at `.cargo/config.toml`:

```
[target.armv7-unknown-linux-gnueabihf]
linker = "arm-none-linux-gnueabihf-gcc"
```

And with this, I can finally compile !

## Final test
Now I can test the produced binary on the target machine, you know, "trust but verify" stuff.
`rsync --progress target/armv7-unknown-linux-gnueabihf/release/rpi pi:~/` and on the pi:

```
pi@raspberrypi:~ $ ./rpi
Hello World!
```

victory !
`<(''<)  <( ' ' )>  (> '')>`

Now I can make a LED blink !
