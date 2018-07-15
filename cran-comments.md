# Submission notes

## Purpose

- Bug fixes and stability enhancements (noted in NEWS).  

## Test environments

* local macOS 10.13.6
* ubuntu Ubuntu 16.04.5 LTS, R 3.5
* Windows Server 2012 R2 x64 (build 9600), R 3.5 (on Appveyor)
* local Windows 10, R 3.5
* win-builder (devel and release)

## R CMD check results

### Note on UBSAN issues

Our package has some recurring UBSAN issues.  These are warnings that occur in RcppParallel, because of code in the TBB (Intel Threading Building Blocks) library used by RcppParallel.  We have been in a wide discussion with the RcppParallel development team (see https://github.com/RcppCore/RcppParallel/issues/36) but they have identified the problem as an object call in TBB.  This seems to have no consequences for stability in packages that use these functions.  One of the RcppParallel developers, Kevin Ushey (kevinushey@gmail.com) has confirmed this.

RcppParallel has the same UBSAN issues (https://www.stats.ox.ac.uk/pub/bdr/memtests/clang-UBSAN/RcppParallel/tests/doRUnit.Rout), as do other packages that use RcppParallel (e.g. gaston: https://cran.r-project.org/web/checks/check_results_gaston.html).

### ERRORs or WARNINGs

None, although see above re: UBSAN.

### NOTES

None.

## Downstream dependencies

No errors, warnings, or notes were caused in other packages, using `devtools::revdep_check()` to confirm.
