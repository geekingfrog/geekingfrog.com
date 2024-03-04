---
title: Video encoding file size tradeofs
status: published
tags:
  - ffmpeg
---

In my [last post](/blog/post/yt-dlp-accurate-chapters) I mentionned how to use
the GPU to re-encode video. [A friend](https://blog.epheme.re/) mentionned that
there is a tradeof regarding file size. GPU encoding is much faster, but
produces files that are also much bigger, around twice as big as the cpu
encoded one.

I ran a quick test on my machine, using this [video](https://www.youtube.com/watch?v=MCYdpIf3tGc), size: 55.2MiB,
originally encoded with VP9.

The main command is roughly:
`ffmpeg -y -loglevel repeat+info -i original.webm -c:v av1 av1_cpu.mkv` to get a cpu transcoding
or
`ffmpeg -y -loglevel repeat+info -vaapi_device /dev/dri/renderD128 -i original.webm -vf 'format=nv12|vaapi,hwupload' -c:v av1_vaapi av1_gpu.mkv`
for gpu transcoding.

```markdown
| video codec | gpu size | cpu size  | gpu time | cpu time |
------------------------------------------------------------
| av1         | 145 MiB  |   X       |    7s    |    (1)   |
| H.265       | 55 MiB   |  24 MiB   |    7s    |   44s    |
| H.264       | 109 MiB  |  54 MiB   |    9s    |   21s    |
| VP9         |   (2)    |  38.7 MiB |   (2)    |   3m37s  |
```

Notes:
1) I didn't complete that. ffmpeg was indicating a speed of around 0.05x, so
  the original 3:50 would take an estimated hour and half and I have better things to do.
2) my gpu doesn't support gpu encoding for VP9. I still wanted to see the file size
  if I re-encoded it.

So that's one thing to keep in mind, whether you value speed of transcoding or
file size.
