---
title: How to : start playing around with virtual machines
tags: vm
status: published
---

So here it is, a post to explain how to set up :
<ol>
	<li>The xen virtual machine hypervisor on a host.</li>
	<li>The network so guests OS are seen as independant and autonomous machine within the local domain.</li>
	<li>Some guest OS on the host.</li>
</ol>
This post is more a memo for myself but it can also be used by others, as long as the configuration is very similar it souldn't be problematic.

<!--more-->I assume you have some basis on Linux and networking but nothing to difficult. My own knowledge is pretty limited but google is your friend.

As a foreword there is two method of virtualization : with and without hardware virtualization. To check that :
<pre class="brush:shell">grep --color vmx /proc/cpuinfo</pre>
If you have a non-empty output, that mean your hardware can support hardware virtualization. Otherwise BIOS patch are needed. When the hardware virtualization is enable guest OS can run like native OS, they are not aware they're virtualized. As for me I didn't enabled this feature since I have not a need for great performance, but if you might want to consider it.