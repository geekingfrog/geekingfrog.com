---
title: Custom livereload with express and socket.io
status: published
tags:
- javascript
- websocket
---

Here's a very quick guide to set up your own livereload server. By livereload, I mean that the browser automatically refresh when some files change. No more Alt-tab F5 Alt-tab \o/
<h2>The server</h2>
Get the right dependencies:
Package.json:

```
npm i express socket.io node-watch --save
```

server.js:

```
app = require('express')() //express server
server = require('http').createServer(app) //http wrapper server
io = require('socket.io').listen(server) //socket.io in charge
server.listen(1234)

// watch .js and .css files
watch = require('node-watch')
filter = function(pattern, fun){
  return function(filename){
    if(pattern.test(filename){fun(filename);}
  }
}

watch('./', filter(function(/\.js|\.css$/i, function() {
  io.sockets.emit('reload') //send a message to all clients
});

//set up express to serve static content
app.use(express.directory(__dirname));
app.use(express.static(__dirname));
```

You can then start the server with `node server.js` and head to http://localhost:1234 You should see the content of the directory where you started the server.
<h2>The client</h2>
The client is ridiculously simple, just add the following snippet in the head of your .html file:

```
<script src="/socket.io/socket.io.js"></script>
<script>
  socket = io.connect('/');
  socket.on('reload', function() { location.reload() });
</script>
```

The socket.io.js script will be served by the http wrapper.
<h2>Conclusion</h2>
In less than 50 lines of code, you can now have a server which will refresh you browser every time you change a file ^^

There are other tool for that, like brunch and grunt, but here I needed this feature on a custom server with already a lot of logic inside. So I added it.