---
title: Some bash tweaks
status: published
tags:
- cli
- bash
---

When you are a basic nerd who wants to always use the command line, or when you are playing with a server (like me) you have to use the command line. I wasn't  too used to the shell, and there were some things which really annoyed me. I'm using bash and I have no idea what are the other shell which can support the following tweaks but that's enough for me.

The first annoying feature was the dumb auto-completion. When there are multiple candidate one has to press  twice to see the list of candidate. Which is highly  consuming.  And because I'm lazy I wanted to cycle through the candidate. Because usually there are only 2-3 it's very quick to select the good one. The following lines shoulb be put in your ~/.inputrc file.


<pre class="brush:shell">#TAB:menu-complete
"e[Z": "e-1C-i"
"t": menu-complete</pre>
Another nice feature is a smart history. For example, if I want to type a SSH command, I start typing "ss" then I press PageUp and it cycles through the previous command beggining with "ss". Also save a lot of time there.
<pre class="brush:shell"># alternate mappings for "page up" and "page down" to search the history
"e[5~": history-search-backward
"e[6~": history-search-forward</pre>
The last feature is a shared history among all the opened terminals. Bash read the .bash_history file only once (at the start) and write the history of executed command when it exists. That means if the shell is killed or crashes, the history is lost. I like to open many terminals and I constantly switch from one terminal to another, but they don't share their history because they load the history only once. The fix is to add the following line in the .bashrc file.
<pre class="brush:shell">export PROMPT_COMMAND="history -a; history -c; history -r;  $PROMPT_COMMAND"</pre>
(I read this tweak <a href="http://briancarper.net/blog/248/">there</a>)

&nbsp;

Now I'm more efficient when geeking with my server :)