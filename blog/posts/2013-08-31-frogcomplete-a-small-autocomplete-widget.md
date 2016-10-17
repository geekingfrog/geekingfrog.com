---
title: FrogComplete, a small autocomplete widget
tags: javascript
status: published
---

So I applied for a position as a web engineer at Mozilla Taiwan to work on firefox OS (a full html5 os to run on smartphone, super cool project), and I got a pre-interview assignment. The goal was to write a small autocomplete widget with some requirements:
<ul>
	<li>Avoid using well-known UI libraries.</li>
	<li>The dataset should contains around 100~1000 entries. (example of dataset provided).</li>
	<li>User should be blocked from submit the input unless the input
matches one of the entries.</li>
	<li>The whole thing should work when loading from <span class="moz-txt-link-freetext">http://localhost/</span> or
file:/// url, without server-side (e.g. PHP) logic.</li>
	<li>Please take care of memory consumption and efficiency.</li>
	<li>Consider making your code unit-testable and reusable.</li>
</ul>
I spent 2-3 days on it, doing my best to have a decent widget, and the result is FrogComplete. The code is on <a href="https://github.com/geekingfrog/frogComplete">github</a>. There is <a href="http://geekingfrog.com/frogComplete/demo/demo.html">live demo</a> and you can find a small overview of the project on my <a href="http://geekingfrog.com/lab.html">lab page</a>.

**edit** Ultimately it didn't work out for Mozilla :( **/edit**