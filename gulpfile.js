'use strict';

var gulp = require('gulp');
var merge = require('merge-stream');
var to5 = require('gulp-6to5');

var to5ops = {
  blacklist: ['forOf', 'letScoping', 'generators']
};

gulp.task('script', function() {
  var server = gulp.src('lib/index.js')
  .pipe(to5(to5ops))
  .pipe(gulp.dest('dist'));

  var jquery = gulp.src('bower_components/jquery/dist/jquery.js')
  .pipe(gulp.dest('dist/javascript'));

  return merge(server, jquery);
});

gulp.task('jade', function() {
  return gulp.src('lib/views/**/*.jade')
  .pipe(gulp.dest('dist/views'));
});

gulp.task('css', function() {

  var normalize = gulp.src('bower_components/normalize.css/normalize.css')
  .pipe(gulp.dest('dist/css'));

  var semantic = gulp.src('bower_components/semantic/build/packaged/*')
  .pipe(gulp.dest('dist'));

  var custom = gulp.src('lib/css/*')
  .pipe(gulp.dest('dist/css'));

  return merge(normalize, semantic, custom);
});

gulp.task('assets', function() {
  return gulp.src('assets/*')
  .pipe(gulp.dest('dist/assets'));
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
