## Submission notes


## Purpose

- Minor bug fixes and improvements. 
- Removed unnecessary import of the **digest** package.

## Test environments

* local OS X install, R 3.4.3
* ubuntu Ubuntu 14.04.5 LTS (on travis-ci), R 3.4.2
* Windows Server 2012 R2 x64 (build 9600), R 3.4.2 (on Appveyor)
* local Windows 10, R 3.4.2
* win-builder (devel and release)

## R CMD check results

### Note on UBSAN issues

Our package has some recurring UBSAN issues.  These are warnings that occur in RcppParallel, because of code in the TBB (Intel Threading Building Blocks) library used by RcppParallel.  We have been in a wide discussion with the RcppParallel development team (see https://github.com/RcppCore/RcppParallel/issues/36) but they have identified the problem as an object call in TBB.  This seems to have no consequences for stability in packages that use these functions.  One of the RcppParallel developers, Kevin Ushey (kevinushey@gmail.com) has confirmed this.

RcppParallel has the same UBSAN issues (https://www.stats.ox.ac.uk/pub/bdr/memtests/clang-UBSAN/RcppParallel/tests/doRUnit.Rout), as do other packages that use RcppParallel (e.g. gaston: https://cran.r-project.org/web/checks/check_results_gaston.html).

### ERRORs or WARNINGs

None, although see above re: UBSAN.

### NOTES

None (on macOS Sierra 10.13.2).

Only this from the results of testing on win-builder.r-project.org:

* checking installed package size ... NOTE
  installed size is  5.4Mb
  sub-directories of 1Mb or more:
    data   1.2Mb
    libs   3.1Mb


## Downstream dependencies

No errors, warnings, or NOTES were caused in other packages, using `devtools::revdep_check()` to confirm.


