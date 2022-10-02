---
title: Ruby on rails
status: published
tags:
- rails
---

Back to the usual stuff here.

I started to <a href="http://blog.geekingfrog.com/?p=64">learn ruby</a> a while ago, and it was only for the sake to try out Ruby on Rails. The first step is to install and prepare everything for rails, which require some tweak because I am using the <a href="https://rvm.beginrescueend.com/">Ruby Version Management</a> tool (rvm). For that, I just followed instructions <a href="http://amerine.net/2010/02/24/rvm-rails3-ruby-1-9-2-setup.html">there</a>, and basically, one has just to set up the rails environment :
<pre class="brush:shell">$ rvm use --create 1.9.2@rails3</pre>
<pre class="brush:shell">and install rails and reguired gems :</pre>
<pre class="brush:shell">$ gem install sqlite3-ruby
$ gem install rails</pre>
make this environment the default :
<pre class="brush:shell">$ rvm 1.9.2@rails3 --default</pre>
and then <a href="http://guides.rubyonrails.org/getting_started.html">start playing with RoR</a>.