---
title: Getting things done with async rust
status: published
tags:
- rust
- async
---

I've been using rust for various side projects for a while now. They tend to
be around web stuff, and the rust ecosystem is pretty much full async for this domain.
I find that it's a bit of a shame, not everyone has to handle a bazillion concurrent
connection, and sometimes doing a blocking http call is completely fine.

But, since things are this way, it's better to know how to tame the unwieldy
beast that is async rust.

I'm not going to describe in details how futures work in rust, there are already [much
better articles](https://fasterthanli.me/articles/understanding-rust-futures-by-going-way-too-deep)
lying around.

What I want is give some pointers for: "I need to do X in an async way, but the interface doesn't
exactly match my need, what do?"

# Miscellaneous useful things

First, a few generic pointers that are very useful.

## `tokio_util` for tokio/futures compatibility layer

[Tokio](https://tokio.rs/) is the most well known async runtime out there. There are entire
frameworks like [axum](https://docs.rs/axum/latest/axum/) designed to work with tokio.
However, there's also a bunch of utilities and traits like `Stream` from the
[futures](https://docs.rs/futures/latest/futures/) crate.
There is a large overlap between the two, and [several](https://docs.rs/async_zip/latest/async_zip/index.html)
[libraries](https://docs.rs/multer/latest/multer/index.html) implements the futures' version
of their traits.

The crate [`tokio_util`](https://docs.rs/tokio-util/latest/tokio_util/compat/struct.Compat.html) comes in
handy with the `compat` feature.

```rust
// this implement the tokio::io::AsyncRead trait
let input_file = tokio::fs::File::open("coucou.txt").await?;

use tokio_util::compat::TokioAsyncReadCompatExt;
let future_version = input_file.compat();
// now this implements futures::io::AsyncRead
```

The crate also has [ReaderStream](https://docs.rs/tokio-util/latest/tokio_util/io/struct.ReaderStream.html)
and [StreamReader](https://docs.rs/tokio-util/latest/tokio_util/io/struct.StreamReader.html) that
turn an `AsyncRead` into a `Stream` and vice versa.

## Extension traits

The basic traits in async land are [`Future`](https://doc.rust-lang.org/std/future/trait.Future.html),
`AsyncRead` ([tokio](https://docs.rs/tokio/latest/tokio/io/trait.AsyncRead.html)/[futures](https://docs.rs/futures/latest/futures/io/trait.AsyncRead.html))
and `AsyncWrite` ([tokio](https://docs.rs/tokio/latest/tokio/io/trait.AsyncWrite.html)/[futures](https://docs.rs/futures/latest/futures/io/trait.AsyncRead.html))
but they are pretty bare-bones. The method they provide are all poll oriented, returning a [Poll](https://doc.rust-lang.org/std/task/enum.Poll.html) and
generally too low level for anything immediately useful.

Tokio and futures provide the `AsyncReadExt` and `AsyncWriteExt` extension traits that add a whole bunch of new methods.
It's worth having a look at them to get an idea of what's available "for free".

## Adapters

Building on the previous point, some of the AsyncExt traits provide adapters,
that extends or modify an existing object. It's similar to the
[BufReader](https://doc.rust-lang.org/std/io/struct.BufReader.html) from the
standard lib which adds buffering to a given reader. The async version exist as
well, but there are many more.

### Example: split a file into chunks

I had a usecase where I had to split in incoming stream of bytes into chunks of at most `n` MiB. The `AsyncReadExt` trait
provides a useful [`take`](https://docs.rs/tokio/latest/tokio/io/trait.AsyncReadExt.html#method.take) method for that.

```rust
use tokio::fs::File;
use tokio::io::AsyncReadExt;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // something big that gives you an AsyncRead
    let input_file = File::open("big_data.bin").await?;

    const CHUNK_SIZE: u64 = 1024 * 1024;

    let mut index = 0;
    // the `.take` method is coming from the AsyncReadExt trait imported above
    let mut reader = input_file.take(CHUNK_SIZE);
    loop {
        let filename = format!("output_{index:05}");
        let mut output = File::create(&filename).await?;
        let bytes_copied = tokio::io::copy(&mut reader, &mut output).await?;

        if bytes_copied == 0 {
            // EOF on the initial reader, remove the empty file we created
            tokio::fs::remove_file(filename).await?;
            break;
        }
        index += 1;
        // our reader is now exhausted, but that doesn't mean the underlying reader
        // is. So we recover it, and we create the next chunked reader
        reader = reader.into_inner().take(CHUNK_SIZE);
    }

    Ok(())
}
```

The pattern `into_inner` to recover ownership of the underlying object from an adapter is pretty common.


# Bridging AsyncRead with async functions

Sometimes though, you need to craft an `AsyncRead`, and [that can be a pain](https://fasterthanli.me/articles/pin-and-suffering).
As an example, let's create an `unsplit` function that does the opposite of the previous example. We take an iterator of filepath,
open each file in turn, and copy its content into one final file.

[`copy`](https://docs.rs/tokio/latest/tokio/fs/fn.copy.html) takes one reader though, so we need a way to create that.
In case we can use `copy` in an async manner, we could simply do the following:

```rust
use std::io::ErrorKind;

use tokio::{fs::File, io::copy};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut index = 0;
    let mut output = File::create("recombined.data").await?;
    loop {
        let filename = format!("output_{index:05}");
        let mut file = match File::open(&filename).await {
            Ok(file) => file,
            Err(err) => {
                if err.kind() == ErrorKind::NotFound {
                    break;
                } else {
                    return Err(err.into());
                }
            }
        };
        copy(&mut file, &mut output).await?;
        index += 1;
    }
    Ok(())
}
```

Running the previous example then this one gives us the same original file back.
But what if `copy` was off the table? I recently needed a way to stream
an http response of unknown size with axum, using [StreamBody](https://docs.rs/axum/latest/axum/body/struct.StreamBody.html).
And for this, I had to construct an AsyncRead.

So instead, we can create a struct where we store the future generated by an async function,
and the manual implementation of `AsyncRead` becomes much easier.

First, let's transform the core async function responsible of copying the files:

```rust
// Regular async function, copy stuff over a generic writer
async fn copy_files<W>(mut output: W) -> Result<(), std::io::Error>
where
    // the Unpin restriction is going to come handy later
    // The compiler will complain and suggest to add it if you forget
    W: AsyncWrite + Unpin,
{
    let mut index = 0;
    loop {
        let filename = format!("output_{index:05}");
        let mut file = match File::open(&filename).await {
            Ok(file) => file,
            Err(err) => {
                if err.kind() == ErrorKind::NotFound {
                    break;
                } else {
                    return Err(err.into());
                }
            }
        };
        copy(&mut file, &mut output).await?;
        index += 1;
    }
    Ok(())
}
```


Now, the trick is to have an in-memory buffer so that we can reuse the `copy_files` function
and use this buffer for the AsyncRead implementation.

```rust
#[pin_project]
struct CombinedReader {
    #[pin]
    reader: DuplexStream,
    future: BoxFuture<'static, std::io::Result<()>>,
    //      ☝️ coming from futures::future::BoxFuture which is a type alias
    // it's equivalent to the line below
    // future: Pin<Box<dyn Future<Output = std::io::Result<()>>>>,
}
```

If you're confused about the `#[pin_project]` bit, [this (very long) article](https://fasterthanli.me/articles/pin-and-suffering)
covers it pretty well. The [module documentation (much shorter)](https://docs.rs/pin-project/latest/pin_project/) is also
very helpful.

Now, we define a constructor:

```rust
impl CombinedReader {
    fn new() -> Self {
        // this is our in-memory buffer
        let (reader, writer) = tokio::io::duplex(4096);
        // *exactly* same function as before, yay reuse
        let future = copy_files(writer);
        Self {
            reader,
            future: Box::pin(future),
        }
    }
}
```

And now, we're ready to implement AsyncRead ourselves, but it's going to be easy.

```rust
impl AsyncRead for CombinedReader {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        // attempt to fill the DuplexStream a bit more if possible
        // poll_unpin comes from futures::FutureExt
        match self.future.poll_unpin(cx) {
            Poll::Ready(Err(err)) => return Poll::Ready(Err(err)),
            // Pending or Ready(Ok()) are both fine and we just carry on
            _ => (),
        };

        // handy macro to simplify the pattern matching over Ready/Pending
        futures::ready!(self.project().reader.poll_read(cx, buf))?;
        Poll::Ready(Ok(()))
    }
}
```


And with this, we can recombine all our files in 3 lines:
```rust
let mut output = File::create("recombined.data").await?;
let mut reader = CombinedReader::new();
copy(&mut reader, &mut output).await?;
```

Et voilà.

Storing a `Pin<Box<dyn Future…>>` is *hugely* beneficial, as we don't have to deal with a
state machine. The compiler does that internally for us.


# Conclusion
Rust's futures basic abstractions are very low level and difficult to use directly.
However, the crates `futures`, `tokio` and others provide many utilities to make
our lives easier.
And as an last resort solution, one can alway store a future in a pinned box, and poll
it manually to integrate a nice `async` function into a low level `Async*` trait implementation.

