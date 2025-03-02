---
title: Wireshark permissions
status: published
tags:
- TIL
---

To run wireshark with your regular user's permission, the following
incantation will do:

```bash
sudo setcap cap_net_raw,cap_net_admin=eip /usr/sbin/dumpcap
```

Also make sure `dumpcap` is in the `wireshark` group: `sudo chgrp wireshark /usr/sbin/dumpcap`.
