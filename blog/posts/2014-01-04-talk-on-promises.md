---
title: Talk on promises
tags: promise, javascript, meetup
status: published
---

Last friday I gave a talk for a <a href="http://www.meetup.com/javascript-enthusiasts/">javascript meetup in taipei</a> about the promise/A+ pattern. The <a href="http://slid.es/geekingfrog/asynchronous-programming-with-promises">presentation</a> is hosted on slid.es
A couple notes:
<ul>
	<li>The font was way too small, making it hard to read (especially for the code).</li>
	<li>People are not familiar with functional programming. I chose to use a functional way for the snippets because that's what coding with promises feel like, but it was more confusing than anything else. So next time stick to the very basis if it's not related to the talk.</li>
	<li>Quite a few questions about jQuery implementation of promises. Yes, jQuery implementation is broken since an exception will blow up instead of putting the promise in "rejected" state. Use Q instead or wrap jQuery promise inside Q promise as soon as possible if this is not acceptable for you (it is acceptable for a lot of people).</li>
</ul>
Overall good experience, people were interested and curious and it improved my understanding of the topic.