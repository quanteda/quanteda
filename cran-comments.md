## Submission notes

NOTE: SUPERCEDES previous 0.99.20 submission, as I discovered a bug related to 
the case of a function, that for some reasons did not show up on previous
tests.

### Purpose

- Bug fixes and stability improvements to existing 0.99.12 CRAN version.
- To ensure compatibility with imminent **testthat** 2.0 package update.


## Test environments

* local OS X install, R 3.4.1
* ubuntu Ubuntu 14.04.5 LTS (on travis-ci), R 3.4.1
* Windows Server 2012 R2 x64 (build 9600), R 3.4.1 (on Appveyor)
* local Windows 10, R 3.4.1
* win-builder (devel and release)

## R CMD check results

### ERRORs or WARNINGs

None.

### NOTES

None (on macOS Sierra 10.12.6).

Only this from the results of testing on win-builder.r-project.org:

* checking installed package size ... NOTE
  installed size is  5.2Mb
  sub-directories of 1Mb or more:
    libs   2.9Mb


## Downstream dependencies

Only the following was revelead by `devtools::revdep_check()` to confirm.

`Checked tidytext      : 1 error  | 0 warnings | 0 notes`

This was because **tidytext** uses a function call that has been deprecated for over a year.  I have issues a [pull request](https://github.com/juliasilge/tidytext/pull/87) for **tidytext** and notified the package maintainers to fix this, nearly three weeks ago.

We are updating our CRAN version because of a messaged from Hadley Wickham that he will soon (13 November) update **testthat** and that our package tests will fail unless we change to the new **testthat** 2.0 syntax.  So while we will break one test in **tidytext**, we will break a **testthat** test in our own package without the update.

## Other

We have tried hard to investigate the UBSan issues indicated from our previous release, and believe that we have found the source of the problem and fixed it.  However if this is not the problem - which we cannot replicate exactly on any of our own tests - then the issue is something in RcppParallel and its interaction with the TBB library and the precise choice of compiler that is used.  On our tests (on the systems above) we have been unable to reproduce the exact issue.
