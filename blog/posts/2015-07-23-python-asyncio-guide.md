---
title: Python asyncio guide (part 1/2)
status: published
tags:
- python
- asyncio
---

# Before python 3.4
The latest realeases of python bring a lot of improvements regarding concurrent programming in python. Prior to 3.4, python had mostly two ways to handle concurrency: [threading](https://docs.python.org/3/library/threading.html) and [multiprocessing](https://docs.python.org/3/library/multiprocessing.html).

Multithreading in python is far from ideal because of the [global interpreter lock](https://wiki.python.org/moin/GlobalInterpreterLock), which prevent more than one thread to be executed at the same time (regardless of the number of core of the machine). Thus, adding more thread is useful only when it comes to blocking IO, where the thread is put to sleep by the os until a condition is met.

Multiprocessing spawn other python process, which means no shared memory. Python handle the "shared" variables by [pickling](https://docs.python.org/3.4/library/pickle.html) them and sending them to the other processes. Thus, it is impractical if there are big shared structures, and they are not truly shared.

For python 2, there is [twisted](https://pypi.python.org/pypi/Twisted) which brings an event loop and tons of protocols and plugins to do things concurrently. Although, twisted has a complex API and a few design mistake, it is still a very robust solution. Moreover it has recently been ported to python 3 \o/.

# Asyncio
This post is going to talk about a lightweight and native python way to do concurrent programming. From 3.4 [asyncio](https://docs.python.org/3/library/asyncio.html?highlight=coroutine#module-asyncio) gives you an event loop, which is perfect to handle long lived connections (think websockets) or IO bound tasks. The main advantage compared to thread is that the memory impact is much lower with asyncio since there is no need to create a new thread for each task.


# The basics
```
import asyncio

loop = asyncio.get_event_loop()

def hello():
	print('hello')
    loop.stop()

loop.call_soon(hello)
loop.run_forever()
loop.close()
```

`run_forever` will start the event loop and execute the scheduled callbacks. This function is interupted by `loop.stop`. At the end, closing the loop will clean up all remaining callbacks (if any).

# Scheduling things
What's nice with an event loop is that scheduling things to run in the future is straightforward.

```
import asyncio

loop = asyncio.get_event_loop()

def hello():
	print('hello')
    loop.call_later(1, hello) # call me again in one second
    
hello()

loop.call_later(2.5, loop.stop)

print('starting event loop')
loop.run_forever
loop.stop()
print('loop stopped')
```
gives:
```
hello
starting event loop
hello
hello
loop stopped
```

# Waiting for an async action

Python 3.4 introduced support for coroutines, which are really similar to generators and are used in asyncio with the `yield from` keywords.

```
import asyncio

loop = asyncio.get_event_loop()

@asyncio.coroutine   # (1)
def long_async_task():
	print('starting long non blocking task')
    yield from asyncio.sleep(3)   # (2)
    print('done with non blocking task')
    return 42

@asyncio.coroutine
def do_something():
	print('need to compute something')
    result = yield from long_async_task()  # (3)
    print('result is', result)

def hello():
	print("See, I'm not blocked")

loop.call_later(2, hello)
loop.run_until_completed(do_something())  # (4)
print('all done')
```

Outputs:
```
need to compute something
starting long non blocking task
(1 second later)
See, I'm not blocked
(2 seconds later)
done with non blocking task
result is  42
all done
```

The first thing to note is the decorator at line `1`: `@asyncio.coroutine`. All coroutine should be decorated like that, for documentation purpose. This cannot be enforced, but since coroutine and functions behaves very differently, it's important to avoid mixing two different things together.
On lines `2` and `3` the new keywords `yield from` are used to suspend the execution of the coroutine until the expression on the right complete. Its result is then returned and the execution of the coroutine resumed.
At line `4`, the event loop is started with a generator as argument. **Calling a coroutine returns a generator**, and this generator is then supplied to the event loop.

# Calling a coroutine in the future
If you try:
```
loop.call_soon(do_something)
```
you get:
```
TypeError: coroutines cannot be used with call_soon()
```

The correct way to use coroutine then is:
```
loop.call_soon(asyncio.async, do_something()) # note the call to do_something here
loop.call_later(1, asyncio.async, do_something())
```

# Conclusion
Asyncio provides an easy and lightweight way to do concurrent programming with python 3.4. The main limitation is that a blocking task run inside the event loop will prevent any other callbacks to be executed. I'll address that in a future post and show how to use threads or processes to run blocking tasks from the event loop.
