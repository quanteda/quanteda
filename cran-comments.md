## Submission notes

This is a significant update in terms of adding functionality, fixing bugs, and creating greater compatibility with other packages.

## Test environments

* local OS X install, R 3.3.2
* ubuntu Ubuntu 14.04.5 LTS (on travis-ci), R 3.3.2
* Windows Server 2012 R2 x64 (build 9600), R 3.3.2 (on Appveyor)
* local Windows 10, R 3.3.2
* win-builder (devel and release)

## R CMD check results

### ERRORs or WARNINGs

None.

### NOTES

None (on macOS Sierra 10.12.6).

From the results of testing on win-builder.r-project.org, I see: 
* checking installed package size ... NOTE
  installed size is  5.2Mb
  
This is our last transitional version before 1.0, and the slightly larger sizes are due to keeping some deprecated items.  We will remove these in the next release, and hope that the slightly larger size is ok for this version.


## Downstream dependencies

No changes in this release affect the (few) downstream packages that Import **quanteda**.
We have run `devtools::devtools::revdep_check()` to confirm.
