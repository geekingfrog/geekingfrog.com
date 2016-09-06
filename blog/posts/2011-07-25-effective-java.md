---
title: Effective java
tags: book, java
status: published
---

I just finished a great book <a href="http://neumino.com">a friend</a> recommended to me about java. Although I don't have the knowledge to understand everything (it's quite complete and complex), I learned a lot of things. Here are some of them :

<strong>Consider static factory methods instead of constructors</strong>. And by declaring a private constructor, one can enforce control over the number of instantiation. It also come in handy for object with a lot of optional parameters with default values.

<strong>Eliminate obsolete reference.</strong> A garbage collector doesn't ensure there is no memory leaks.

<strong>Obey the general contract when overriding equals</strong>. This one is tricky and I won't detail it here, it's a remainder for myselfif I do that later.

<strong>Design and document for inheritance or else prohibit it</strong>. It's simple to inherit something, but can be difficult to support for in the long term, and even more difficult to chage if it's part of an API.

<strong>Don't use raw type in new code</strong>, and <strong>prefer list to array</strong>. For similar reasons, various errors which stem from casting object are detected at compile time.

<strong>Prefer for-each loops to traditionnal loops</strong>. With three exceptions : when one wants to delete <em>some</em> elements, <em>transform</em> <em>some</em> elements, or do parallel iteration over multiple loops.

<strong>Know and use the libraries</strong>. When I was young, I wanted to understand everything, and so I wanted to do everything by myself. Now I'm wiser, but still, this item is really important. Here is a little example from the book :
<pre class="brush:java">private static final Random rnd = new Random();
// Common but deeply flawed!
static int random(int n) {
return Math.abs(rnd.nextInt()) % n;
}</pre>
Which has 3 flaws (I only found one and half when I first read it)

<strong>Optimize judiciously</strong>. Write good programs rather than fast ones. Because if it's too slow at the end, a good architecture allow ones to optimize the program easily.

<strong>Prefer empty arrays or collections, not null</strong>. Special case should not be handled using null elements.

<strong>Prefer executors and tasks to threads</strong>. Better abstractions then thread exist now to do background tasks and manage queue. And, remember to <strong>document thread safety </strong>(immutable, unconditionally thread safe, conditionally thread safe, not thread safe, thread hostile).<strong>
</strong>

&nbsp;

I can go on for other items but I think it's enough for the time. I recommend this book for any object programmer who wants to write better code. It's not a book for beginner, but even if one is not a master of java, or just doesn't know java very well, it's still very useful.