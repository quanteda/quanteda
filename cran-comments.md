# Submission notes

## Purpose

- Bug fixes, stability enhancements, and feature additions (noted in NEWS).
- Fixed some URLs that became broken due to changes on the target websites.

## Test environments

* local macOS 10.14, R 3.5.1
* ubuntu Ubuntu 16.04.5 LTS, R 3.5.1
* Windows Server 2012 R2 x64 (build 9600), R 3.5.1 (on Appveyor)
* local Windows 10, R 3.5.1
* win-builder (devel and release)

## R CMD check results

### Note on UBSAN issues

Our package has some recurring UBSAN issues.  These are warnings that occur in RcppParallel, because of code in the TBB (Intel Threading Building Blocks) library used by RcppParallel.  We have been in a wide discussion with the RcppParallel development team (see https://github.com/RcppCore/RcppParallel/issues/36) but they have identified the problem as an object call in TBB.  This seems to have no consequences for stability in packages that use these functions.  One of the RcppParallel developers, Kevin Ushey (kevinushey@gmail.com) has confirmed this.

We are far from the only package to be affected by these (harmless) compiler warnings, for instance the following are also affected:

* RcppMeCab - https://www.stats.ox.ac.uk/pub/bdr/gcc8/RcppMeCab.out
* gaston — https://www.stats.ox.ac.uk/pub/bdr/memtests/gcc-UBSAN/gaston/build_vignettes.log
* RcppParallel - https://www.stats.ox.ac.uk/pub/bdr/memtests/gcc-UBSAN/RcppParallel/RcppParallel-Ex.Rout

We hope that this will not block the updating of our package, and also expect that as TBB improves (from Intel) and this filters into RcppParallel, these warnings will eventually go away.


### ERRORs or WARNINGs

None, although see above re: UBSAN.

### NOTES

None.

## Downstream dependencies

No errors, warnings, or notes were caused in other packages, using `devtools::revdep_check()` to confirm.
