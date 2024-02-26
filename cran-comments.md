# Submission notes

*  Major update versus v.3.3.1, with many new features and improvements -- see NEWS.
*  Numerous bug fixes.
*  Numerous compatibility enhancements with newer versions of some packages (e.g. Matrix).


## Test environments

* local macOS 14.2.1, R 4.3.2
* Ubuntu 22.04 LTS, R 4.3.2
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* Windows oldrelease via devtools::check_win_oldrelease()

## R CMD check results

All checks are fine, locally and on GitHub's CI for multiple platforms.  But `devtools::check_win_release()` and `devtools::check_win_devel()` shows the following:

```
* checking tests ... [146s] ERROR
  Running 'spelling.R' [0s]
  Running 'testthat.R' [145s]
Running the tests in 'tests/testthat.R' failed.
Complete output:
  > Sys.setenv("R_TESTS" = "")
  > Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = TRUE)
  > 
  > library(testthat)
  > library(quanteda)
  Package version: 4.0.0
  Unicode version: 15.1
  ICU version: 74.1
  Parallel computing: 2 of 56 threads used.
  See https://quanteda.io for tutorials and examples.
  > 
  > # for strong tests for Matrix deprecations
  > options(Matrix.warnDeprecatedCoerce = 2)
  > 
  > ops <- quanteda_options()
  > quanteda_options(reset = TRUE)
  > test_check("quanteda")
```

We have tried to examine the results of `examples_and_tests/tests/testthat.Rout.fail` but that file produces a 404 error. 

I emailed Uwe Ligges about this following a previous submission, and he sought input from Tomas Kalibera, who tested the package and identified the issue as pertaining to a version of the TBB library that is part of RpppParallel specific to the ARM64 platform for windows.

He wrote:
"please release a new revision with the fixes you have on github."


## Reverse dependency and other package conflicts

According to revdepcheck::revdep_check(), this breaks the **gofastr** package because that package calls functions that we first deprecated in version 3.3 and have now made defunct.  We issued a pull request fixing this for that package over four months ago (https://github.com/trinker/gofastr/pull/12) and the package maintainer has yet to accept it.
