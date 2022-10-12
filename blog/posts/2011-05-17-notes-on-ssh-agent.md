---
title: Notes on ssh agent
status: published
tags:
- ssh
- server
---

While I was installing my svn server, I learnt that sometimes, the svn command like commit establishes multiple connections. Because I am using svn over ssh, each connection prompt the user to enter the password, which is very annoying. It was the perfect occasion to discover what ssh agent is.

Basically it's a daemon which establish a ssh connection to a host, and then handle all other ssh connection as subprocesses. For the user, it means enter your password once and for all, and then use ssh freely :) This is done by the use of a pair of public/private key. The public key is added to the .ssh/authorized_key file in the remote machine and the private key is used by the agent to establish the connection. Now I enter the passphrase once and I can establish as many ssh connection I want without beeing annoyed by a password prompt. More details on ssh agent <a href="http://mah.everybody.org/docs/ssh">here</a>.