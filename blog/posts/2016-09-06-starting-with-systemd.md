---
title: Starting with systemd
tags: systemd, linux, devops
status: published
---


# The harbringer of doom
When I think of systemd, I think of:

![systemd is still hungry](http://i3.kym-cdn.com/photos/images/newsfeed/000/925/966/8d2.gif)

Although the gif is hilarious I tend to agree that one abstraction to rule
them all is likely a road to disaster. But that's a flame war for another day.

# Casual usage of systemd

So I just rebuilt my website, and took the chance to upgrade my server as well.
Now I'm running Debian 8.5 and with it, systemd.

So my primary question was: *How do I run my webapp with systemd ?*

Turns out it's pretty simple. First, create a **systemd service file**: `geekingfrog.service`

```
[Unit]
Description=Geekingfrog website
After=local-fs.target

[Service]
WorkingDirectory=/home/greg/website
ExecStart=/home/greg/website/geekingfrog 8080

[Install]
WantedBy=multi-user.target
```

and copy this file to `/lib/systemd/system/geekingfrog.service`. A symlink won't work.
With a symlink, trying to enable the service will give:

```
Failed to execute operation: No such file or directory
```

To check if the service is correctly picked up by systemd:

```
systemctl status geekingfrog.service
```

To start it at system boot, it needs to be enabled, and then manually started:

```
systemctl enable geekingfrog.service
systemctl start geekingfrog
```


# Useful links

Some useful links to start with systemd:

* [Arch linux wiki](https://wiki.archlinux.org/index.php/Systemd). Always very good documentation
* [systemd(1) man page](https://www.freedesktop.org/software/systemd/man/systemd.unit.html) wich links
  to more detailed man pages: `systemd.unit(5)` and `systemctl(1)`
* [znc doc to run as daemon](http://wiki.znc.in/Running_ZNC_as_a_system_daemon).
  Very straightforward example with basic skeleton.
* [How to use systemctl](https://www.digitalocean.com/community/tutorials/how-to-use-systemctl-to-manage-systemd-services-and-units) from the DigitalOcean tutorial. Loads of good and practical stuff there. Kuddo to them.
