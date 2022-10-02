---
title: Unix permissions using node
status: published
tags:
- node
- javascript
---

Small snippets to get file permissions using node.
[`node.stat`](http://nodejs.org/api/fs.html#fs_fs_stat_path_callback) returns an object with a field `mode`. The mode contains the file information in decimal format.
Then, from `man 2 stat`, one get the masks:

```
0170000   bit mask for the file type bit fields
0140000   socket
0120000   symbolic link
0100000   regular file
0060000   block device
0040000   directory
0020000   character device
0010000   FIFO
0004000   set-user-ID bit
0002000   set-group-ID bit (see below)
0001000   sticky bit (see below)
00700     mask for file owner permissions
00400     owner has read permission
00200     owner has write permission
00100     owner has execute permission
00070     mask for group permissions
00040     group has read permission
00020     group has write permission
00010     group has execute permission
00007     mask for permissions for others (not in group)
00004     others have read permission
00002     others have write permission
00001     others have execute permission
```

So finally, if one want to know if one can write to a file:

```
var mode = require('fs').statSync(file).mode;
console.log(
  (mode & 00200) !== 0
  ? 'can write'
  : 'no write permissions'
);
```