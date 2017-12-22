## Submission notes

RESUBMISSION: Removes the "http://doi.org" from the doi in CITATION (as instructed 
following earlier submission of v0.9.22).

### Purpose

- Bug fixes and stability improvements to existing 0.99.22 CRAN version.
- Prepare for imminent transition soon to v1.0


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

Only the following was revelead by `devtools::revdep_check()` to confirm.

`Checked tidytext      : 1 error  | 0 warnings | 0 notes`

This was because **tidytext** uses a function call that has been deprecated for over a year.  I have issues a [pull request](https://github.com/juliasilge/tidytext/pull/87) for **tidytext** and notified the package maintainers to fix this, nearly two months ago.


