---
title: Installing guest OS using XEN
status: published
tags:
- vm
---

Recently I am working on OS virtualization and load balancing. To do some test I have to install an Hypervisor (xen) to manage virtual machines (VM) on hosts. I am just at the beginning and it's a quite tedious work. I am using Debian Squeeze with Xen 4.0.1. Because Xen is still under development, there is no clear and up-to-date documentation.

I spent the afternoon trying to install a guest OS using virt-install or the graphical tool virt-manager but I was getting the following error :
<pre class="brush:shell">ERROR    unable to connect to 'localhost:8000': Connection refused</pre>
After searching for network problems (bridge configuration of xen) and security problem (/etc/hosts.allow or /etchosts.deny) I finnally found this :
<pre class="brush:shell">#(xend-unix-server no)</pre>
in the /etc/xen/xend-config.sxp and change it to :
<pre class="brush:shell">(xend-unix-server yes)</pre>
And I got rid of the previous error.

Next : set up these damn VM