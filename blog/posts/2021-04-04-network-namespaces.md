---
title: nope
status: draft
tags:
- nope
---




with the vpn:
```
❯ ip link
1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN mode DEFAULT group default qlen 1000
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
2: enp5s0: <BROADCAST,MULTICAST> mtu 1500 qdisc noop state DOWN mode DEFAULT group default qlen 1000
    link/ether 90:2b:34:32:7a:ea brd ff:ff:ff:ff:ff:ff
3: wlp8s0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP mode DORMANT group default qlen 1000
    link/ether 00:08:ca:a9:d1:b8 brd ff:ff:ff:ff:ff:ff
4: ppp0: <POINTOPOINT,MULTICAST,NOARP,UP,LOWER_UP> mtu 1354 qdisc fq_codel state UNKNOWN mode DEFAULT group default qlen 3
    link/ppp
```

what I have:
```
$ ip route
default dev ppp0 scope link
111.111.111.111 via 192.168.0.1 dev wlp8s0
169.254.2.1 dev ppp0 proto kernel scope link src 10.212.134.65
192.168.0.0/24 dev wlp8s0 proto dhcp scope link src 192.168.0.14 metric 3003
```

alexises:
```
$ ip route
default dev ppp0 scope link
111.111.111.111 via 192.168.0.1 dev wlp8s0
169.254.2.1 dev ppp0 proto kernel scope link src 10.212.134.65
192.168.0.0/24 dev wlp8s0 proto dhcp scope link src 192.168.0.14 metric 3003


# basic route of your os
default via via 192.168.0.1
192.168.0.0/24 dev wlp8s0 proto dhcp scope link src 192.168.0.14 metric 3003

# what your vpn done
$ ip route
default dev ppp0 scope link
111.111.111.111 via 192.168.0.1 dev wlp8s0
169.254.2.1 dev ppp0 proto kernel scope link src 10.212.134.65
192.168.0.0/24 dev wlp8s0 proto dhcp scope link src 192.168.0.14 metric 3003

#what you need
# basic route of your os
default via via 192.168.0.1
<lan-copo-1/cidr> dev ppp0 scope link
<lan-copo-2/cidr> dev ppp0 scope link
<lan-copo-3/cidr> dev ppp0 scope link
169.254.2.1 dev ppp0 proto kernel scope link src 10.212.134.65
192.168.0.0/24 dev wlp8s0 proto dhcp scope link src 192.168.0.14 metric 3003
```

without vpn: the routes are:
````
❯ ip route
default dev ppp0 scope link
167.98.110.77 via 192.168.0.1 dev wlp8s0
169.254.2.1 dev ppp0 proto kernel scope link src 10.212.134.58
192.168.0.0/24 dev wlp8s0 proto dhcp scope link src 192.168.0.14 metric 3003
````

vpn routing table:
```
            - 192.168.1.0/24
            - 192.168.9.0/24
            - 192.168.4.0/24
            - 192.168.55.0/24
            - 192.168.56.0/24
            - 192.168.59.0/24
            - 192.168.60.0/24
            - 192.168.64.0/24
            - 192.168.65.0/24
            - 192.168.86.0/24
            - 192.168.89.0/24
            - 192.168.90.0/24
            - 192.168.91.0/24
            - 192.168.94.0/24
            - 192.168.95.0/24
```
