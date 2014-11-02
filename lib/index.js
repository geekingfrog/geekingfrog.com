'use strict';

var env = process.env.NODE_ENV || 'dev';

var koa = require('koa');
var jade = require('koa-jade');
var serve = require('koa-static');
var compress = require('koa-gzip');
var conditional = require('koa-conditional-get');
var etag = require('koa-etag');

var logger = require('./logger');

var app = koa();
app.use(compress());
app.use(function* (next) {
  this.response.vary('Content-Encoding');
  yield next;
});

app.use(conditional());
app.use(etag());

app.use(jade.middleware({
  viewPath: __dirname+'/views',
  basedir: __dirname,
  pretty: env === 'dev',
  compileDebug: env === 'dev',
  noCache: env === 'dev'
}));

app.use(require('koa-trie-router')(app));

app.use(serve('./dist', {
  maxage: 1000 * 3600 * 24 // one day
}));

var fetchLatestPosts = require('./fetchBlog');

// to cache the result
var latestPosts;

app.route('/')
.get(function* () {
  yield this.render('index', {posts: latestPosts});
});

app.route('/ping').get(function* () {
  logger.debug('pong');
  this.body = 'pong';
});

var port = process.env.PORT || 3000;

var retrievePosts = function* () {
  latestPosts = yield fetchLatestPosts();
  return latestPosts
};

require('co')(function*() {
  yield retrievePosts();

  // poll latest posts every hours
  setInterval(require('co')(retrievePosts), 1000 * 3600);

  app.listen(port, function(err) {
    if(err) {
      logger.fatal('Cannot start server', err);
      setTimeout(() => process.exit(1) , 200);
      return;
    }
    logger.info('Server listening on port %d with env %s', port, env);
  });
})();
