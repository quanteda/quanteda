## Submission notes

Bug fixes and stability improvements to existing 0.99 CRAN version.

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

From the results of testing on win-builder.r-project.org, I see: 
* checking installed package size ... NOTE
  installed size is  5.4Mb
  sub-directories of 1Mb or more:
    libs   3.1Mb
    
This is our last transitional version before 1.0, and the slightly larger sizes are due to keeping some deprecated items.  We will remove these in the next release, and hope that the slightly larger size is ok for this version.


## Downstream dependencies

No changes in this release affect the (few) downstream packages that Import **quanteda**.

We have run `devtools::revdep_check()` to confirm.
