## Submission notes

### Resubmission

Following a submission on 2017-01-12, we had some UBSAN issues flagged by the CRAN team.  These are warnings that occur in RcppParallel, because of code in the TBB (Intel Threading Building Blocks) library used by RcppParallel.  We have been in a wide discussion with the RcppParallel development team (see https://github.com/RcppCore/RcppParallel/issues/36) but they have identified the problem as an object call in TBB.  This seems to have no consequences for stability in packages that use these functions.  One of the RcppParallel developers, Kevin Ushey (kevinushey@gmail.com) has confirmed this.

RcppParallel has the same UBSAN issues (https://www.stats.ox.ac.uk/pub/bdr/memtests/clang-UBSAN/RcppParallel/tests/doRUnit.Rout), as do other packages that use RcppParallel (e.g. gaston: https://cran.r-project.org/web/checks/check_results_gaston.html).

#### What we did fix:

In investigating all of the code very carefully and all of the issues reported in the UBSAN tests, we did find and fix:

`quanteda.h:131:30: runtime error: shift exponent 32 is too large for 32-bit type 'unsigned int'`

(from https://www.stats.ox.ac.uk/pub/bdr/memtests/gcc-UBSAN/quanteda/tests/testthat.Rout)

In our tests, this eliminated
```
vptr for 'tbb::internal::custom_scheduler<tbb::internal::IntelSchedulerTraits>'
/data/gannet/ripley/R/test-3.5/RcppParallel/include/tbb/task.h:946:28: runtime error: member call on address 0x7eff104afa00 which does not point to an object of type 'scheduler'
0x7eff104afa00: note: object is of type 'tbb::internal::custom_scheduler<tbb::internal::IntelSchedulerTraits>'
 00 00 00 00  70 3b d8 10 ff 7e 00 00  3c 55 0f 04 20 60 00 00  00 00 00 00 00 00 00 00  00 3c 4a 10
```
but we do not think it will eliminate the TBB-related issues, which we cannot solve at the level of our package.

#### Note on (using the same) version number

We would prefer to keep the v1.0.0 if that's okay, since this is a big milestone release for us.


### Purpose

- Major new release: With the "1.0" designation, it means the package API will be stabilized for future development.  Many additions, improvements to function consistency, stability enhancements, and bug fixes.  
- Fixes a problem noted by Brian Ripley on 2018-01-06 with regard to the order of the compiler flags in `src/Makevars`.


## Test environments

* local OS X install, R 3.4.3
* ubuntu Ubuntu 14.04.5 LTS (on travis-ci), R 3.4.2
* Windows Server 2012 R2 x64 (build 9600), R 3.4.2 (on Appveyor)
* local Windows 10, R 3.4.2
* win-builder (devel and release)

## R CMD check results

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


