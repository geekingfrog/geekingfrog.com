---
title: ssh and identy files
tags: ssh
status: published
---

Out of frustration, and maybe it'll be useful for another one.
I just spent 6 hours trying to battle a nasty issue with ssh keys. I have a lot of different keys on my machine, and after unsucessfully trying 5 keys, ssh will drop the connection.
The usual solution is to specify which host should use which key in an **ssh config file**. For example:

```
Host github.com
  IdentityFile ~/.ssh/github_rsa
```
The file must have permission `600` otherwise it'll be ignored.
For some host which constantly change (like amazon instances), a better solution is to use the `-i` command to manually specify the private key to use.

### Here's the catch
`-i` doesn't mean use **only** this key (or use this key **first**). The implementation is actually to *try the given key **after** all keys from the agent failed*. Which is **insane**.

One needs to add the option `IdentitiesOnly` to disable all key from the agent.
So the successful command would be:
`ssh -i ~/path/to/my/key -o IdentitiesOnly=yes user@host`

The man page for ssh is really confusing on this -,..,-


Thanks to [this post](http://sealedabstract.com/code/github-ssh-with-multiple-identities-the-slightly-more-definitive-guide/) which gave me the solution