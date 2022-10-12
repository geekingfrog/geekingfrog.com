---
title: Three flaws in software design
status: published
tags:
- software-design
- meta
---

Google posted a [small serie of videos](https://www.youtube.com/watch?v=JOAq3YN45YE) about some flaws in software design. I found it really interesting as it relates to the day to day life to any developer. Here's my quick takeaway.

### Don't write code that isn't needed.
It's similar to the MVP in the lean startup theory. Don't write code that does not bring value immediately. There was a great example about a youtube-like app. Don't start by writing the browsing feature, because, in itself that's useless. Start by writing the video player. That's the core of the application.

### Don't write code that is not easy to change
There was a lot of great examples coming from the bugzilla project. Basically, treat every asumptions like *users will never need to do that* as a warning. Be careful of magic numbers littering the code everywhere. One might be right in these asumptions, but most likely, he will be wrong.

### Don't be too generic (overengineering)
This point is hard to balance with the previous one. There was a great advice about it: the system should look like it was design from the start to do what it's doing. The example was about a socket implementation.

You start with a simple unix socket, with 2 classes: `Socket` and `SocketTest`. Then, when you want to add a UDP socket, you don't add an `if` statement in the `Socket` class. At that point, you create an abstract class with `UnixSocket` and `UDPSocket` as concrete implementation. And so on, when you have to add the TCP socket feature, you can then pull out the IP layer.

### Some generic advices
The last part was about some final thoughts. And especially, two things I'll remember.

* Design with unit test in mind **from the start**. It's really really hard to add unit testing in a codebase not designed for that. (I'm struggling with that every day at the day job).
* Internationalization is **hard** to add later on. But don't build the whole framework from the start. Just add a simple layer which will make the addition of international support later easier.

The talks were made by a book author and I am now seriously considering reading his book, there was a lot of value in his talks.
Go through the videos if you have one hour to spare.