var gulp = require('gulp'),
    shell = require('gulp-shell')

gulp.task('run-tests', shell.task([
  'elm make AllTests.elm --output test.js',
  'node test.js'
]))


gulp.task('default', function () {
  gulp.run('run-tests')

  gulp.watch(['../src/**/*.elm', './**/*.elm'], ['run-tests'])
})
