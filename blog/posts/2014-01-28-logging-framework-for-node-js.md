---
title: Logging framework for node.js
status: published
tags:
- rsyslog
- log4js
---

I spent two days refactoring our 'logging' system. It was broken because of a poor override of the default console object.

So now, we're using [log4js-node](https://github.com/nomiddlename/log4js-node) as the logging framework and it works great! When I chose log4js, I thought it was more actively maintained than the venerable [winston](https://github.com/flatiron/winston). It turned out this is not the case. Both are very similar and pretty neat.

The goal is to have our console message sent to the system `syslog`. In amazon cloud, we're using [loggly](https://www.loggly.com/) to get these logs. Anyway, as soon as there is more than one server involved, one has to think how to get all of these logs in place. And the `rsyslog` system works pretty well. Fortunately, there is already an appender using `ain2`: `log4js-ain2`.

On ubuntu, there is a few configurations from the default to enable this though. The config file is in `/etc/rsyslog.conf`.
One has to uncomment the following lines:

```
$ModLoad imudp
$UDPServerRun 514

#...
$ModLoad imtcp
$InputTCPServerRun 514
```
and restart: `sudo /etc/init.d/rsyslog restart` the service.
Also, by default, `rsyslog` transform *weird* characters (ASCII<32) so the output is on one line. Example:

```
Feb 26 08:03:37 greg-Desktop /home/greg/projects/log4js-ain2/node_modules/ain2/index.js[11274]: [default] -  [Error: BOOM]#012Error: BOOM#012    at Object.<anonymous> (/home/greg/tests/log4jsTest/index.js:44:14)#012    at Module._compile (module.js:456:26)#012    at Object.Module._extensions..js (module.js:474:10)#012    at Module.load (module.js:356:32)#012    at Function.Module._load (module.js:312:12)#012    at Function.Module.runMain (module.js:497:10)#012    at startup (node.js:119:16)#012    at node.js:902:3
```

To fix that and get more readable messages, still in the same file:
```
#
# Don't escape ASCII<32 characters (newlines and tabs)
#
$EscapeControlCharactersOnReceive off
```

and after a restart:
```
Feb 26 08:03:25 greg-DigbilDesktop /home/greg/projects/log4js-ain2/node_modules/ain2/index.js[11231]: [default] -  [Error: BOOM]
Error: BOOM
    at Object.<anonymous> (/home/greg/tests/log4jsTest/index.js:44:14)
    at Module._compile (module.js:456:26)
    at Object.Module._extensions..js (module.js:474:10)
    at Module.load (module.js:356:32)
    at Function.Module._load (module.js:312:12)
    at Function.Module.runMain (module.js:497:10)
    at startup (node.js:119:16)
    at node.js:902:3
```

Which is much better.