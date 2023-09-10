---
title: Protohackers learnings
status: draft
tags:
- server
- networking
---

# Protohackers ?

[Protohackers](https://protohackers.com/) is a very cool website presenting
several network programming problems. So far, I've done 11 out of 12 problems
and it was a fun experience with a bunch of things I learn.
Each problem describe a protocol to implement, and then you give an IP address
with a port, and an automated checker will test your implementation.

The neat thing about protohackers is that the challenges are fairly realistic, many
of them are simplified version of real world protocols and services. For example
there's a [chat server](https://protohackers.com/problem/3), a
[man in the middle](https://protohackers.com/problem/5), a simplified
[tcp over udp](https://protohackers.com/problem/7) and a simplified
[encryption protocol](https://protohackers.com/problem/8).
I quite like this aspect since one can draw inspiration from the real things.

Because you only provide an IP address and the port, you can do these challenges
with any programming language of your choice. If it can handle tcp and udp socket,
binary and text data (with a dash of json here and there), you're good to go.
I did these in rust and they're [on github](https://github.com/geekingfrog/protohackers).

Following is a list of things I learned, some useful, some surprising, in no
particular order.

# Details matter, a lot
When doing network programming, it is imperative to get the details right.
A classic example is [read_line](https://docs.rs/tokio/latest/tokio/io/trait.AsyncBufReadExt.html#method.read_line)
may or may not return a newline character at the end, if EOF has been reached. This is often
important. Sending one byte wrong, especially if it's a delimiter between frame or
some other control byte can be the difference a sucess and a client hanging until time out.

Another important detail that trip me up was [read_exact](https://docs.rs/tokio/latest/tokio/io/trait.AsyncReadExt.html#method.read_exact)
that reads until the given buffer is full. This is fairly common to use that for length
prefixed protocols. The following however, will always immediately returns with
an empty buffer:

```rust
// we expect to read 32 bytes
let mut buf = Vec::with_capacity(32);
input.read_exact(&mut buf).await?;
```

However, `with_capacity` may allocates, but doesn't change the length of the buffer, so
effectively, the buffer is empty and cannot hold anything. The correct thing to do is

```rust
// Correct way to initialize the buffer
let mut buf = vec![0; 32];
input.read_exact(&mut buf).await?;
```


# Put timeouts everywhere

These are annoying, but it is quite important to think about timeout from the very
beginning. What happens if a client suddenly goes away? What happens if a client
is criminally slow to send data?
For many problems such paranoia was vastly overkill, but I found it a good exercise.
It's especially important to put timeout in tests, it provides a nicer experience
when debugging a hanging server.

# Channels are very good !

This was perhaps the most surprising to me. A typical server is one task owning a
[TcpListener](https://docs.rs/tokio/latest/tokio/net/struct.TcpListener.html) and
[spawns](https://docs.rs/tokio/latest/tokio/task/fn.spawn.html) tasks for clients when new
connections arrive.

I started using Mutex and references between clients and server for communications and
synchronisation. However, that turned out to be awkward and fiddly.
Communication between central state and tasks handling client was particularly thorny,
because often one needs some kind of async operation like broadcasting a message to other
tasks. And doing so while holding a lock is a recipe for deadlock and/or grumpy compiler.

So I turned to another architecture, where the central state isn't shared anywhere and
not held within a Mutex. When a new connection arrives, the server spawns a client, giving
it a clone of a [mpsc::Sender](https://docs.rs/tokio/latest/tokio/sync/mpsc/struct.Sender.html)
to communicate with the server.

Most of my servers ended up looking roughly like:

```rust
struct Server {
    // to give a clone to every new Client
    server_tx: tokio::sync::mpsc::Sender<Message>,

    // recv on this channel to process commands from the
    // tasks managing connections.
    server_rx: tokio::sync::mpsc::Receiver<Message>,
}

tokio::select!(
    loop {
        tokio::select! {
            // handle new connection
            x = self.listener.accept() => {
                let (stream, addr) = x?;
                tracing::info!("new client connecting from {addr}");
                // spawns a new client with a way to communicate with the server
                Client::new(stream, addr, self.server_tx.clone()).spawn_run();
            }

            // handle commands coming from the clients
            Some(cmd) = self.server_rx.recv() => {
                self.handle_command(cmd)?;
            }
        }
    }
)
```

And when the clients needs a response from the central server, for example, the ID
of the job that was enqueued like in [the job centre problem](https://protohackers.com/problem/9)
the message sent holds a [mpsc::oneshot::Sender](https://docs.rs/tokio/latest/tokio/sync/oneshot/struct.Sender.html)
and the client can then awaits on the response:

```rust
enum Message {
    // notify the server that there's a new job to enqueue
    // the details of the job are omitted here for brevity
    NewJob{assigned_id: oneshot::Sender<u64>}
}

let (tx, rx) = oneshot::channel();
// notify the server
self.server_tx.send(Message{assigned_id: tx}).await?;
let new_id = rx.await;
// and now, we can answer the remote client with the ID for the job
// they have just submitted.
```

This pattern turns out to be very nice to use. It's a tad verbose because it requires
a new `enum` and variants for every type of messages to be exchanged between server and
tasks. However, this scales very well, and once the base is in place, adding messages
is straightforward.

I may expand on this idea in a later post. The gist of it is that message passing + share
nothing architecture works stupidly well for network code.


# Compartimentalize mutable state

Even more than usual, as soon as async is involved, holding onto many mutable references
becomes very tricky across await points. For example, the following is not possible:

```rust

impl Server {
    async fn run(&mut self) {
        tokio::select!(
            conn = self.accept() => { todo!() },
            cmd = self.client_command() => { todo!() },
        )
    }

    async fn accept(&mut self) { todo!() }
    async fn client_command(&mut self) { todo!() }
}
```

This doesn't compile because `accept` and `client_command` both take `&mut self`.
Thankfully, you can use a `&mut self` method inside the branches. So to compile the
previous example should be:

```rust
impl Server {
    async fn run(&mut self) {
        tokio::select!(
            conn = self.listener.accept() => { self.handle_accept(conn).await },
            cmd = self.chan.recv() => { self.handle_client_command(cmd).await },
        )
    }

    async fn handle_accept(&mut self, conn: …) { todo!() }
    async fn handle_client_command(&mut self, cmd: …) { todo!() }
}
```

This works because, once a branch is chosen, the many multiple borrows in select are
"released".

This is where the [TcpStream::split](https://docs.rs/tokio/latest/tokio/net/struct.TcpStream.html#method.split)
and [TcpStream::into_split](https://docs.rs/tokio/latest/tokio/net/struct.TcpStream.html#method.into_split) methods
come into play, allowing one to share the read and write parts of a tcp stream without
having to manually share a mutable borrow.

This forces you to think hard about what is mutable and what isn't, which is a
very rust thing, and not necessarilly bad, but can be annoying sometimes.


# Nom nom nom parsing

Parsing is always an important part in these challenges, if only because that's the first
thing you need to do when receiving data.

I relied heavily on [nom](https://docs.rs/nom/latest/nom/index.html) for the parsing.
Sometimes it was clearly overkill, where regexp could have worked equally well.
However, contrary to regexps, nom allow to build composable and extensible parsers
which is very nice. Also, nom can handle incomplete input, which came handy for one
challenge, and works equally well for text or binary data.

The downsides of nom are that it's a bit verbose. I like to put the parsing inside
a submodule which only exports one or two function and keep the nom imports and
details hidden.
Also, the error handling can be complicated if you want something a bit custom.
(I may write something more elaborate on this topic later.)


# Think about clients as well

Even when the task is only about developping a server, it's quite handy to have a
simple client ready to use as well. Especially for testing, once you're reasonably sure the
low level details are fine and you want to test more complex interactions, it cuts down the
verbosity of tests by a lot.


# Some special mentions

## [line reversal](https://protohackers.com/problem/7)
I found this problem to be the toughest. It's a (much) simpler version of quic/tcp over udp.
It's quite interesting but also complex, because you need to implement yourself what the kernel
usually do for you. This lead to having multiple tasks per connections, interacting together.
Very satisfying to get right though.

## [speed daemon](https://protohackers.com/problem/6)
This is the first problem where there are a lot of functional requirements with some subtle interactions.
I found the most annoying was to have clients that could request heartbeat before or after identifying
themselves as camera or dispatcher. That lead to some code duplication. I'm not sure what's the best way
to deal with these kind of state machine (yet?)


# Conclusion

Overall it was quite fun and educational. I liked the flexibility where you can
make it as simple or as over-engineered as you'd like. The automatic checker
also does test for some modicum of performance as well as many edge cases. It
is also cool that all edge cases are mentionned, but sometimes somewhat hidden
in the text.
