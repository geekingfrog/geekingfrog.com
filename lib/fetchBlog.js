'use strict';

var thunkify = require('thunkify');
var request = thunkify(require('request'));
var cheerio = require('cheerio');
var blogUrl = 'http://blog.geekingfrog.com';

module.exports = function* fetchLatestPosts() {
  var content = yield request(blogUrl);
  content = content[1];
  var $ = cheerio.load(content);
  var posts = [...$('article.post')];
  return posts.map( post => {
    post = $.load(post);
    var title =  post('.post-title a').text();
    var link = blogUrl + post('.post-title a').attr('href');
    var date = post('time').text();
    return {title, link, date}
  });
}