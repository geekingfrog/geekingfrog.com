---
title: Split youtube video's chapter accurately
status: published
tags:
  - linux
  - ffmpeg
  - yt-dlp
---

# Yt-dlp and chapters

My main source of music is (*gasp*) youtube, and often I put very long videos in
the background. For example: this [civilization
VI](https://www.youtube.com/watch?v=fZA7ARptg0M) arrangement.

One thing I do sometimes is to download the video, split by chapters, and
randomise the order of the tracks with `mpv --shuffle`. For this, the following
incantation will yield something almost correct:

``` 
yt-dlp "https://www.youtube.com/watch?v=fZA7ARptg0M" \
    --no-vid \
    --verbose \
    --split-chapters \
    --output "civ6-$(playlist_index)s-$(title)s.%(ext)s"
```

This yields a list of file with names following the given template, but they
don't *quite* match the chapter boundaries. This is because of how the video is
encoded, the underlying `ffmpeg` cannot split at other points than keyframes.


# Audio only, easy

For audio, it's simple enough since the encoders aren't that expensive to run:

```
yt-dlp "https://www.youtube.com/watch?v=fZA7ARptg0M" \
    --no-vid \
    --verbose \
    --split-chapters \
    --force-keyframes-at-cut \
    --output "civ6-$(playlist_index)s-$(title)s.%(ext)s"
```

The option `--force-keyframes-at-cut` will tell ffmpeg to re-encode the file
and then split. This takes around 3 minutes on my machine.


# With video !

Now, that's another story.

Unfortunately, changing the keyframes requires re-encoding the video, which is
*very* resource intensive. And using only the CPU for that can take an
extremely long times (many hours).

This is where hardware acceleration comes in. Modern GPU comes with support for
native decoding *and* encoding 😍. I'm using linux and a gpu AMD, the
[wiki](https://wiki.archlinux.org/title/Hardware_video_acceleration) has pretty
good documentation about setting these things up.

For example, running `vainfo` gives me:
```
Trying display: wayland
Trying display: x11
vainfo: VA-API version: 1.20 (libva 2.20.1)
vainfo: Driver version: Mesa Gallium driver 23.3.4-arch1.2 for AMD Radeon RX 7800 XT (radeonsi, navi32, LLVM 16.0.6, DRM 3.56, 6.7.2-arch1-1)
vainfo: Supported profile and entrypoints
      VAProfileH264ConstrainedBaseline:	VAEntrypointVLD
      VAProfileH264ConstrainedBaseline:	VAEntrypointEncSlice
      VAProfileH264Main               :	VAEntrypointVLD
      VAProfileH264Main               :	VAEntrypointEncSlice
      VAProfileH264High               :	VAEntrypointVLD
      VAProfileH264High               :	VAEntrypointEncSlice
      VAProfileHEVCMain               :	VAEntrypointVLD
      VAProfileHEVCMain               :	VAEntrypointEncSlice
      VAProfileHEVCMain10             :	VAEntrypointVLD
      VAProfileHEVCMain10             :	VAEntrypointEncSlice
      VAProfileJPEGBaseline           :	VAEntrypointVLD
      VAProfileVP9Profile0            :	VAEntrypointVLD
      VAProfileVP9Profile2            :	VAEntrypointVLD
      VAProfileAV1Profile0            :	VAEntrypointVLD
      VAProfileAV1Profile0            :	VAEntrypointEncSlice
      VAProfileNone                   :	VAEntrypointVideoProc
```

`VAEntrypointVLD` means support for decoding this format, and
`VAEntrypointEncSlice` means support for encoding.

A quick test taken [from this
page](https://wiki.archlinux.org/title/FFmpeg#Hardware_video_acceleration) is
to run:

```
ffmpeg -vaapi_device /dev/dri/renderD128 \
    -i input.mp4 \
    -vf 'format=nv12,hwupload' \
    -c:v hevc_vaapi \
    -f mp4 \
    -rc_mode 1 \
    -qp 25 \
    output.mp4
```

To force `yt-dlp` to use hardware support I needed to add:

`--postprocessor-args "-vaapi_device /dev/dri/renderD128 -vf 'format=nv12,hwupload' -c:v av1_vaapi"`

Let's dissect that, with the help of the [ffmpeg
documentation](https://trac.ffmpeg.org/wiki/Hardware/VAAPI#Encoding)

* `-vaapi_device /dev/dri/renderD128`. I had two entries under `/dev/dri/`, but
  following `/dev/dri/by-path/` and cross-referecing with the output of `lspci`
  I could disambiguate.

* `-vf 'format=nv12,hwupload'`. `hwupload` passes frames to the encoder as
  VAAPI surfaces. However, the hardware usually wants `nv12` layout, but
  running `ffprobe` on the input file reveals it's using `yuv420p`, hence the
  `nv12` filter.

* `-c:v av1_vaapi` tells ffmpeg to output a video using the `av1` encoder. The
  video format is `webm`, and (apparently) this only supports "vp8, vp9 or
  av1". Since I don't have a native vp9 encoder, I'll go with av1 (remember I have
  `VAProfileAV1Profile0 : VAEntrypointEncSlice`)


The final incantation to accurately split the chapters of a given video are:


```
yt-dlp \
  --verbose \
  "https://www.youtube.com/watch?v=fZA7ARptg0M" \
  --split-chapters \
  --force-keyframes-at-cuts \
  --postprocessor-args "-vaapi_device /dev/dri/renderD128 -vf 'format=nv12,hwupload' -c:v av1_vaapi"
```




# Testing

To check if it's actually working, I manually ran the ffmpeg command doing the
transcoding. In verbose mode `yt-dlp` will output the full command, and running
it in a terminal will give a progress bar. The key point is that the playback
speed should be far greater than 1. On my machine it was around 10x and it took
about 10 minutes to transcode the video.

Also, using `radeontop` I could see some activity on the gpu, that I couldn't
see if I omit the `--postprocessor-args`, as in this later case, everything is
done on the CPU.


# Additional formats

`yt-dlp --list-formats <url>` gives a list of available formats, and then one
can download the preferred combo. For example: `yt-dlp -f 123+456` would
download the format `123` for the video and `456` for the audio (or vice
versa). This way, one can control what comes in before transcoding.
