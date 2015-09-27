'use strict';

if(!process.env.NODE_ENV) process.env.NODE_ENV = 'dev';
var env = process.env.NODE_ENV;

var express = require('express');
var jade = require('jade');

var app = express();

if(env === 'dev') {
  var config = {watchDir: process.cwd()+'/lib'};
  require('express-livereload')(app, config);
}

app.set('views', __dirname+'/views');
app.set('view engine', 'jade')
// app.use(require('morgan')('combined'))
app.use(express.static(__dirname + '/public'))

app.get('/', function(req, res) {
  res.render('index');
});

var port = process.env.PORT || 3000;
app.listen(port);
