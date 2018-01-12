## Submission notes

Fixes a problem noted by Brian Ripley on 2018-01-06 with regard to the order of the compiler flags in src/Makevars.

### Purpose

- Many additions, improvements to function consistency, stability enhancements, and bug fixes.
- With the "1.0" designation, it means the package API will be stabilized for future development.
- Fixes the src/Makevars problem notes above.


## Test environments

* local OS X install, R 3.4.3
* ubuntu Ubuntu 14.04.5 LTS (on travis-ci), R 3.4.2
* Windows Server 2012 R2 x64 (build 9600), R 3.4.2 (on Appveyor)
* local Windows 10, R 3.4.2
* win-builder (devel and release)

## R CMD check results

### ERRORs or WARNINGs

None.

### NOTES

None (on macOS Sierra 10.13.2).

Only this from the results of testing on win-builder.r-project.org:

* checking installed package size ... NOTE
  installed size is  5.4Mb
  sub-directories of 1Mb or more:
    data   1.2Mb
    libs   3.1Mb


## Downstream dependencies

No errors were caused in other packages, using `devtools::revdep_check()` to confirm.

We saw one warning in the **preText** package, because of an incorrect call to a **quanteda** function.  I notified the package maintainer of this in early December.


