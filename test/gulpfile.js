var gulp = require('gulp'),
    shell = require('gulp-shell')

gulp.task('run-tests', shell.task([
  'elm-test AllTests.elm'
]))


gulp.task('default', function () {
  gulp.run('run-tests')

  gulp.watch(['../src/**/*.elm', './**/*.elm'], ['run-tests'])
})
