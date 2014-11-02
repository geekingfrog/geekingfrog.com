'use strict';

var gulp = require('gulp');
var merge = require('merge-stream');
var to5 = require('gulp-6to5');
var imagemin = require('gulp-imagemin');
var pngcrush = require('imagemin-pngcrush');
var minifyCss = require('gulp-minify-css');
var concat = require('gulp-concat');

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
  var png = gulp.src('assets/*.png')
  .pipe(pngcrush({reduce: true})())
  .pipe(gulp.dest('dist/assets'));

  var svg = gulp.src('assets/*.svg')
  .pipe(imagemin())
  .pipe(gulp.dest('dist/assets'));

  return merge(png, svg);

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
