---
title: Keyboard control for mpv(2)
tags: mpv, linux
status: published
---

A while ago, I [posted](./keyboard-control-mpv) my setup to control mpv through keypresses. It was done through JSON IPC.

# MPRIS d-bus interface

I recently learned about the [MPRIS D-Bus](https://specifications.freedesktop.org/mpris-spec/latest/) interface. It stands for Media Player Remote Interfacing Specification, and this is exactly what I was looking for. The problem with JSON IPC is that it would only support one player instance. That is, if I had one instance of mpv playing something, and then start another instance, the keyboard control would not work anymore after the second instance exited.

To get it to work on arch, there are two [aur](aur.archlinux.org/) packages: `mpv-mpris` and `mpris2controller-git`. `mpv-mpris` is an mpv addon to implement the d-bus interface for mpv. `mpris2controller` is a user daemon to monitor mpris enabled programs so that commands are always sent to the correct application.

# I3 config
Finally, my updated i3 config is now:

```sh
bindsym XF86AudioPlay  exec "mpris2controller PlayPause"
bindsym XF86AudioPause exec "mpris2controller PlayPause"
bindsym XF86AudioStop  exec "mpris2controller PlayPause"
bindsym XF86AudioPrev  exec "mpris2controller Previous"
bindsym XF86AudioNext  exec "mpris2controller Next"
bindsym $mod+Control+n exec "mpris2controller Next"
bindsym $mod+Control+p exec "mpris2controller Previous"
bindsym $mod+Control+m exec "get_music_info"
```

The script `get_music_info` is a small utility to retrieve some metadata and send a notification. It's useful when you're listening to something random and think "oh, that's cool, what is this track?".

```bash

#!/usr/bin/env bash

# this requires mpris2controller (mpris2controller-git from aur) mpv-mpris (also from aur)

set -euo pipefail

data=$(mpris2controller Metadata)
notif=$(echo $data | jq -r '.["xesam:title"] + " [" + (.["xesam:artist"] // [] | join("foo")) + "]"')

notify-send "$notif"
```
