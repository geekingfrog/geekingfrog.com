---
title: Faster SSL
tags: 
status: draft
---

Upgrading openssl on ubuntu following [these instructions](http://sandilands.info/sgordon/upgrade-latest-version-openssl-on-ubuntu).
Install nginx from source to get the latest version (1.7.3). For this, a pcre library is required (in order to use mod-rewrite). `sudo apt-get install libpcre3 libpcre3-dev`.

## TLS session ticket
```
$ openssl s_client -connect geekingfrog.com:443 -tls1 -tlsextdebug -status
SSL-Session:
    Protocol  : TLSv1
    Cipher    : ...
    Session-ID: 24A067A10B127B058C9B0F43D63D80F6A9E5D1E2EC5357750ABE551789BC2220
    Session-ID-ctx: 
    Master-Key: ...
    Key-Arg   : None
    PSK identity: None
    PSK identity hint: None
    SRP username: None
    TLS session ticket lifetime hint: 300 (seconds)
    TLS session ticket:
    0000 - 80 eb 5f 0a af ec 05 79-a8 78 9b 1a 9d 4a 6a 53   .._....y.x...JjS
	......
```
The line Session-ID shows that TLS session resumption is enabled. The ticket timeout is 300 seconds, which is a bit short. To fix that, in the nginx confgiruration: `ssl_session_timeout 1d`.
Since I'm running a single server, I don't have to really care about the pitfals of such deployment (requires shared cache and updates otherwise).


## TLS connection
![](/static/images/2014/12/tlsConnection.jpg)
The rtt to my website from my location is around 310ms. So connecting in 618ms means I need two rtt, one of them for the SSL handshake, which is the best setup yeah!

There is a problem though, since going to `http://geekingfrog.com` will redirect to `https://`, and though wasting one RTT.



