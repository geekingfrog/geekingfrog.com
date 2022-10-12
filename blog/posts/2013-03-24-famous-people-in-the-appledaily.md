---
title: Famous people in the appledaily
status: published
tags:
- life
- javascript
---

I just put online a small project to visualize how many articles are about famous taiwanese people. **edit** The project has been taken down since I need some resources back on my server.**/edit** <a href="https://github.com/geekingfrog/twPeople">The code</a> is on github, and below is a copy of the readme which explains some things:

&nbsp;

<p>This fun project is here to visualize the passion (really, the number of article) about some famous taiwanese people. The list of people is on <a href="http://en.wikipedia.org/wiki/List_of_Taiwanese_people">wikipedia</a>.</p>

<p>The project is in two parts.</p>

<h3>The crawler</h3>

<p>For the moment I only crawled the appledaily (蘋果日報), the crawler is in the folder crawler. It is built using node.js and store the data with mongoDB. The code to access the DB (the dao) is in the folder mongo.</p>

<h3>The visualization</h3>

<p>The server and the pages are in the folder app. The server uses node.js and express. The visualization uses the awesome library <a href="http://d3js.org/">d3</a> and basic html/css.</p>

<h3>How do I build this ?</h3>

<p>This project is not really intended to be built and used by other people. You'll want to change the database configuration (in mongo/mongoConfig.coffee) and probably some other things. Maybe I'll make that easier if the future (unlikely).</p>

<h3>Why ?</h3>

<p>This project is only for fun and for the sake of learning new things. Here, it was node.js and mongo (and refresh my memory about d3). This is not a great project but <a href="http://fuckitship.it/">ship early</a>.</p>

<h3>And later ?</h3>

There are tons of things to improve, here are some.

* The crawler is slow, tweak node to open more connections.
* Sometimes the crawler hangs, add a timeout and a retry process.
* The database is not super efficient (count() in mongo is super slow).
* On a slow machine db requests can take up to 10s ! Warm up the cache at startup.
* Put the 'cache' in the dao and impove it.
* Make the crawler fetch new articles every days.
* Crawl other websites: 自由時報， 中國時報，臺灣時報。。。
* Find better color for the graph.
* Make the site less ugly.
