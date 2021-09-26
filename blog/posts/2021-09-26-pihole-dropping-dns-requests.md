---
title: Investigating dropped DNS requests with pi-hole
tags: networking
status: published
---

# What's wrong with my program?

For `$dayjob` I have a few convenient scripts that I maintain. One of them is a helper to gather a bunch of statistics around our usage of AWS SQS. Namely, to be able to see which deadletter queue have messages in them.

The tool is written in rust, mostly because it's fast (noticeable difference with python) and [clap](https://github.com/clap-rs/clap), the library I use for command line argument parsing provides neat autocompletion.

Now, this tools runs fine in debug mode, but in release mode, it would list all our deadletter queues, and then hangs for a couple of seconds before exiting normally. It is this small wait that really puzzled and annoyed me, and that led me to almost a day of digging to find out why and fix it.


# What is blocking?

The first step was to figure out what was the cause of this pause at the very end of the program. My intuition was that there were some blocking tasks in the background which would prevent the runtime (tokio) to properly shut down.

I asked around, and a tentative response was to direct me to the [tokio console](https://github.com/tokio-rs/console) project. After some fiddling around, I managed to install it and instrument my program with it. One thing stoud out: *a lot* of lines like so:

```
fn=<hyper::client::connect::dns::GaiResolver as [omitted]>
```

Now I had a lead, there was something fishy with dns name resolution!


# Debugging DNS queries

I am running [pi hole](https://pi-hole.net/) on a raspberry pi at home, which means my main DNS is somewhat custom, so I suspected that the problem was there. To confirm it, I changed my main DNS to `1.1.1.1`, re-ran my program and there was no problem.
So now, I *knew* that my DNS was the culprit.

Next, I ran wireshark locally to see if that would give me any lead. With the capture filter `udp port 53` to restrict it to DNS query, I got many hits. Some requests were taking almost exactly 5 seconds to complete, but, more precisely, the first request was timing out, before being retried. Clicking on `Follow > Follow UDP stream` I can isolate one such stream:

![isolating one UDP stream](/static/2021-09-26-pihole-dropping-dns-requests/wireshark01.png)

The reproductible 5 seconds was clearly a client timeout. When I reduced the concurrency in my script, the issues also went away. So somehow, the DNS was dropping some requests.
I also ran `tcpdump` on the raspberry pi itself to compare the trace.
One helpful change was to modify under `/etc/dnsmasq.d/01-pihole.conf` the line `log-queries=extra`. By default there's no `extra`.

With the `log-queries=extra` and a restart of the DNS, I could see some logs:

```
Sep 26 10:53:26 dnsmasq[2709]: 673 192.168.1.197/45043 query[A] sqs.eu-west-1.amazonaws.com from 192.168.1.197
Sep 26 10:53:26 dnsmasq[2709]: 673 192.168.1.197/45043 cached sqs.eu-west-1.amazonaws.com is <CNAME>
Sep 26 10:53:26 dnsmasq[2709]: 673 192.168.1.197/45043 cached eu-west-1.queue.amazonaws.com is 63.34.60.197
```

The `extra` bit adds the source IP with the port. And with that, I can compare with the wireshark capture on both machines. I noticed that some queries were received by both machines, but would never show up in the dnsmask logs. That means some packets were being dropped.


# Debugging UDP

Now, to check packet dropped: `netstat -suna` gives:

```
pi@raspberrypi:~ $ netstat -suna
IcmpMsg:
    OutType3: 22
Udp:
    1505 packets received
    31 packets to unknown port received
    85 packet receive errors
    1508 packets sent
    85 receive buffer errors
    0 send buffer errors
    IgnoredMulti: 12
```

Note the line `85 packet receive errors`. That shows the kernel gets the udp packets alright, but then, something happens and dnsmasq never see them. Considering it only happens when I "hammer" the dns, it's probably a buffer filling up.

There are a few commands to check some linux networking knobs:
```
sysctl net.core.rmem_default # 180224
sysctl net.core.rmem_max # 180224
sysctl net.core.netdev_max_backlog # 1000
sysctl net.ipv4.udp_rmem_min # 4096
```
`man udp` helps gives some sense to some of these parameters. Otherwise, duckduckgo is your friend.

I lost *a ton* of time tweaking these parameters to no avail. Turn out, I *must* restart the dns for it to take into account the new values `-_-"`

Increasing the default and max memory for receive buffers from 18Kb to 1MiB fixed the issue:

```
sudo sysctl -w net.core.rmem_default=1048576
sudo sysctl -w net.core.rmem_max=1048576
```

Btw, to make these change persistent across reboot, you need to place a file under `/etc/sysctl.d/` ending in `.conf` with the content:

```
net.core.rmem_default = 1048576
net.core.rmem_max = 1048576
```

# Conclusion

Now, running my tool doesn't increase the number of packet receive error, and everything is as snappy as it should `\o/`.
Of course, the reality wasn't as straightforward as the writeup. I checked the dns logs first, before starting to suspect that some queries were dropped and never even seen by dnsmasq.


# Resources & useful commands

* The man page of dnsmasq. Since it's not available on the system using raspberry pi, [an online version](https://thekelleys.org.uk/dnsmasq/docs/dnsmasq-man.html) works as well.
* [Find out where you're dropping packets](https://jvns.ca/blog/2016/08/24/find-out-where-youre-dropping-packets/) from the ever excellent Julia Evans' blog. That's where I found the `netstat` incantation.
* `tcpdump -i eth0 udp port 53 -w dnscapture.pcap` to capture the trafic on the pi.
* `pihole restartdns` and `pihole -t` to restart the dns and tail its logs.
