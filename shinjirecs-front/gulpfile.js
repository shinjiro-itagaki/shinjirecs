var gulp = require('gulp');
var sass = require('gulp-sass');
var scssdir = "./assets/scss/";
// Sass compile task

gulp.task('scss', function(){
  gulp.src(scssdir + '**/*.scss')
    .pipe(sass())
    .pipe(gulp.dest('./src/css/'));
});

// watch task( watch **/*.scss files)
gulp.task('scss-watch', ['scss'], function(){
  var watcher = gulp.watch(scssdir + '**/*.scss', ['scss']);
  watcher.on('change', function(event) {
    console.log('File ' + event.path + ' was ' + event.type + ', running tasks...');
  });
});

gulp.task('default', ['scss']);