'use strict';

var thunkify = require('thunkify');
var request = thunkify(require('request'));
var cheerio = require('cheerio');
var blogUrl = 'http://blog.geekingfrog.com';
var logger = require('./logger');

module.exports = function* fetchLatestPosts() {

  if(process.env.NODE_ENV === 'dev') {
    logger.debug('returning fake blog data for dev');
    return [
      {title: 'post1', link: 'http://localhost/', date: '03 september 2014'},
      {title: 'post2', link: 'http://localhost/', date: '03 september 2014'},
      {title: 'post3', link: 'http://localhost/', date: '03 september 2014'},
      {title: 'post4', link: 'http://localhost/', date: '03 september 2014'},
      {title: 'post5', link: 'http://localhost/', date: '03 september 2014'},
      {title: 'post6', link: 'http://localhost/', date: '03 september 2014'},
      {title: 'post7', link: 'http://localhost/', date: '03 september 2014'},
      {title: 'post8', link: 'http://localhost/', date: '03 september 2014'}
    ]
  }

  try {
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
  } catch(err) {
    logger.error('Error while fetching latest blog posts:', err);
  }
}
