---
title: Keyboard control for mpv
tags: mpv, linux
status: published
---

# Control mpv from a keypress from anywhere

One of the thing which "just works" on macOS is the binding of play/pause key to mpv. So even without the focus on the terminal running mpv, it's possible to play/pause with a keypress. Here's how to get that on linux.

## MPV config

In `~/.config/mpv/mpv.conf`

```
# JSON IPC
input-ipc-server=~/.config/mpv/socket
```

## i3 config

You need `socat` installed for that as well.

```
bindsym XF86AudioPlay  exec echo cycle pause   | socat - ~/.config/mpv/socket
bindsym XF86AudioPause exec echo cycle pause   | socat - ~/.config/mpv/socket
bindsym XF86AudioStop  exec echo cycle pause   | socat - ~/.config/mpv/socket
bindsym XF86AudioPrev  exec echo playlist_prev | socat - ~/.config/mpv/socket
bindsym XF86AudioNext  exec echo playlist_next | socat - ~/.config/mpv/socket
```

## finding keys

To find out how to bind a specific key, use `xev`, with some grepping because it can be very noisy.
