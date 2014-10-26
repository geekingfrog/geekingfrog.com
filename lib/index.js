'use strict';

var env = process.env.NODE_ENV || 'dev';

var koa = require('koa');
var app = koa();
var jade = require('koa-jade');
var serve = require('koa-static');

app.use(jade.middleware({
  viewPath: __dirname+'/views',
  basedir: __dirname,
  pretty: env === 'dev',
  compileDebug: env === 'dev',
  noCache: env === 'dev'
}));

app.use(require('koa-trie-router')(app));

app.use(serve('./dist'));
app.use(serve('./assets'));

app.route('/')
.get(function *() {
  yield this.render('index');
});

var port = process.env.PORT || 3000;
app.listen(port);
