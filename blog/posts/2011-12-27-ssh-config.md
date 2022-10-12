---
title: ssh config
status: published
tags:
- ssh
---

Apparently I like ssh, because here is another post about it. This is a quick note after I encountered this error when I was trying to connect to some machine at work :
<pre>Received disconnect from 192.168.1.223: 2: Too many authentication failures</pre>
Which is very cryptic. The problem has to do with ssh-agent. When trying to connect, the agent use all keys in store to try to connect. If one has many keys (I have 6 or 7 stored) and if the right key is the last one used, the remote host shut down the connection after <em>n</em> failed attempts. Because it's not always possible to change the number of try on a remote machine to connect, the solution has to be local.

Hence the config ssh file. This file has to be in ~/.ssh/ with permissions set to 600, otherwise it's going to be ignored. The man page of ssh_config describe everything one needs to know about it. Below a quick snippet for the most useful configuration :

```
Host geekingfrog , 178.32.96.76
HostName geekingfrog.com
IdentityFile ~/.ssh/geekingfrog.com
User user_name

Host 192.168.1.226
IdentityFile ~/.ssh/226_dsa
User user_name2
```

Which solve the problem. In the same time, it allow one to uses host alias and username. So `ssh user@my.domain.com` becomes `ssh mydomain`. Also, the hostname and user in the config file also works with scp. \o/