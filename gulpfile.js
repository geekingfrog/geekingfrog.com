'use strict';

var gulp = require('gulp');
var merge = require('merge-stream');
var through = require('through2');
var to5 = require('gulp-6to5');
var changed = require('gulp-changed');
var imagemin = require('gulp-imagemin');
var pngcrush = require('imagemin-pngcrush');
var minifyCss = require('gulp-minify-css');
var concat = require('gulp-concat');
var source = require('vinyl-source-stream');

// node 0.11 --harmony supports these out of the box
var to5ops = {
  blacklist: ['forOf', 'letScoping', 'generators']
};

gulp.task('script', function() {
  // server
  return gulp.src('lib/**/*.js')
  .pipe(to5(to5ops))
  .pipe(gulp.dest('dist'));
});

gulp.task('jade', function() {
  return gulp.src('lib/views/**/*.jade')
  .pipe(gulp.dest('dist/views'));
});

gulp.task('css', function() {

  var paths = [
    'bower_components/normalize.css/normalize.css',
    'lib/css/*'
  ]

  return gulp.src(paths)
  .pipe(minifyCss())
  .pipe(concat('geekingfrog.css'))
  .pipe(gulp.dest('dist/css'));
});

gulp.task('assets', function() {
  var dest = 'dist/assets';
  var png = gulp.src('assets/*.png')
  .pipe(changed(dest))
  .pipe(pngcrush({reduce: true})())
  .pipe(gulp.dest(dest));

  var svg = gulp.src('assets/*.svg')
  .pipe(changed(dest))
  .pipe(imagemin())
  .pipe(gulp.dest(dest));

  return merge(png, svg);

});

var jade = require('gulp-jade');
gulp.task('prepareCritical', ['build'], function() {

  var posts = [
    {title: 'post1', link: 'http://localhost/', date: '03 september 2014'},
    {title: 'post2', link: 'http://localhost/', date: '03 september 2014'},
    {title: 'post3', link: 'http://localhost/', date: '03 september 2014'},
    {title: 'post4', link: 'http://localhost/', date: '03 september 2014'},
    {title: 'post5', link: 'http://localhost/', date: '03 september 2014'},
    {title: 'post6', link: 'http://localhost/', date: '03 september 2014'},
    {title: 'post7', link: 'http://localhost/', date: '03 september 2014'},
    {title: 'post8', link: 'http://localhost/', date: '03 september 2014'}
  ];

  var compileJade = gulp.src('lib/views/index.jade')
  .pipe(jade({
    posts: posts,
    pretty: true,
    basedir: __dirname+'/dist'
  }))
  .pipe(gulp.dest('.critical/'))

  var moveAssets = gulp.src(['dist/**/*.css', 'dist/assets'])
  .pipe(gulp.dest('.critical/'))

  return merge(compileJade, moveAssets);

});

var critical = require('critical');
gulp.task('critical', ['prepareCritical'], function(cb) {

  critical.generate({
    base: './.critical/',
    src: 'index.html',
    minify: true,
    // firefox Flame
    width: 480,
    height: 854
  }, function(err, res) {
    if(err) return console.error('error:', err);
    require('fs').writeFile('dist/css/critical.css', res, cb);
  });
});

gulp.task('watch', function() {
  gulp.watch('lib/css/*', ['css']);
  gulp.watch('lib/views/*', ['jade']);
  gulp.watch('lib/**/*.js', ['script']);
  gulp.watch('assets/*', ['assets']);

  // return gulp.watch('lib/**/*', ['default']);
});

gulp.task('build', ['script', 'jade', 'css', 'assets']);

gulp.task('default', ['build', 'watch']);

gulp.task('prod', ['build', 'critical']);
