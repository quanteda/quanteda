## Submission notes

Thhis is a resubmission to fix errors on the Solaris tests in v0.9.9-22.  These were due to compiler warnings/errors due to unsigned integer comparisons, now resolved.

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

There were 3 NOTES:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Kenneth Benoit <kbenoit@lse.ac.uk>'

Days since last update: 5

Possibly mis-spelled words in DESCRIPTION:
  toolset (4:31)
  
* checking installed package size ... NOTE
  installed size is  7.8Mb
  sub-directories of 1Mb or more:
    data   2.3Mb
    doc    2.3Mb
    libs   2.3Mb 
 
* checking for GNU extensions in Makefiles ... NOTE
GNU make is a SystemRequirements.


## Downstream dependencies

No changes in this release affect the (few) downstream packages that Import **quanteda**.
