---
title: Why I dislike OS X
tags: rant, os-x
status: published
---

It's going to be three months that I've been working on a macbook pro, here are some of my thoughts.

# The good
It's a very good laptop. Even though I'm mostly using it as a desktop, I can see how good of a laptop it is. The **trackpad** is excellent. That's a shame I don't like to use a mouse (that's probably the influence of vim).
The power managament is also very good.
OS X is also POSIX compliant which means most of my beloved cli tools work out of the box, good thing.

# The bad
Upon setup, one has to setup an appleID in order to update any software. And for this, a valid **credit card number** is required. Seriously??? Just to update some free software, I need to hand over my credit card number? That's a giant no.
The **finder** is pretty bad too. First, showing hidden file is [somewhate complex](http://osxdaily.com/2009/02/25/show-hidden-files-in-os-x/). But worse, even with this option enabled, popup to select files still won't show them. Which means, there is **no way** to upload a hidden file from a browser without first renaming it.

# The ugly
* Terrible keyboard support. This is a major pain for me. The trackpad is very good, but if one decides to minimize its use, it's a living hell. There is no `home/end` keys. Which is a major inconvenience. Sometimes some keybindings act like these keys, but they are inconsistent through applications.

* The **application switcher** (cmd+tab) switch between applications. There is a different shortcut to switch between window of the same application. So moving fast between a browser and two terminals is cumbersome. The shortcut to switch between windows of the same application has also a **different** behavior from the application switcher. It cycles linearly through all windows. So for example with 3 windows: A, B, C: `cmd+~ => A -> B (release keys) cmd+~ => B -> C` whereas the application switcher will go back to the first application: `cmd+tab => W1 -> W2 (release keys) cmv+tab => W2 -> W1`. This is a major wtf!

* The **applications path**. The main way to install application is done by drag&drop the icon of the downloaded app to a folder (first wtf). This put the app under `.../Application/<app name>/<app bin>`. So these applications have to been manually added one by one to the `PATH` variable to launch them from the terminal. They can be launched with `cmd+space` which works very well though. The other way to install app is homebrew, which is a soso package manager like `apt` or `yum`. Applications installed this way are available through the terminal, but not with `cmd+space`. Again, lack of consistency here is a massive pain.

# Conclusion
The window manager is horrible for power users and the way to manage applications is terrible. Apple is also not giving users ways to customize their system (changing window manager?). I'm amazed to see most developers in the startup world are using a mac. It's way better than windows (outside the .Net world of course), but I feel the linux world is soooo much better when it comes to development. I'm currently considering my options to install arch linux as a dual boot now.
