---
title: The awesome linux bash
tags: bash, linux
status: published
---

I'm a big fan of unix and bash, the idea of having tons of small tools that can be combined to achieve complex result is really neat.
Recently I had to download a big list of files from amazon s3. I ended up doing:

```bash
cat <(cat list_of_file.txt) <(ls downloaded_files) | sort | uniq -u | xargs -I {} -P6 aws s3 cp s3://bucket_name/sub/directory/full/of/files/{} ./downloaded_files
```

Some explanation (for my future self)

* `<(...)` is a bash command to do process substitution. Which means the output of the command inside the parens will be used as the input for the outside command.
* `cat <(...) <(...) | sort | uniq -u` outputs the files from the list which are not already downloaded.
* `xargs -I {} -P6 [cmd]` run the given command with a parallelism of 6 and will substitute `{}` with the given arguments.

For more resources:

* [command line tool can be faster than hadoop cluster](http://aadrake.com/command-line-tools-can-be-235x-faster-than-your-hadoop-cluster.html)
* [the art of command line](https://github.com/jlevy/the-art-of-command-line). There are so much more in this document, it's well worth multiple reads
* [the mighty named pipe](http://vincebuffalo.com/2013/08/08/the-mighty-named-pipe.html) explains named pipe and process substitution. I'm rarely using these but it's super powerful.