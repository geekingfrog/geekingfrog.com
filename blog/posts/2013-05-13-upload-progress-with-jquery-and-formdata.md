---
title: Upload progress with jquery and FormData
tags: coffeescript
status: published
---

After quite a bit of searching, I found a way to track the upload progress when posting some stuff through FormData. In this case, I assume a recent browser with FormData and xhr2.

```
# get an original xhr object
xhr = $.ajaxSettings.xhr()

# add the listener for progress
if xhr.upload
  xhr.upload.addEventListener "progress", (ev) ->
    # do some stuff using ev.position and ev.totalSize

# get a file to upload
file = $("#file")[0].files[0]
form = new FormData()
form.append('file', file)

# upload the file using jquery.ajax
upload = $.ajax
  url: 'upload'
  type: 'POST'
  xhr: -> xhr
  data: form
  contentType: false # prevent jquery to mess up with
  processData: false # the upload by trying to serialize the payload

upload.done ->
  console.log "file uploaded ^^"
```

And that's it. For more complete version, plugins exists (with iframe transport and all that jazz in case of a paleo-browser)