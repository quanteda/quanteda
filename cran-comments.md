# Submission notes

RESUBMISSION: Fixed a test that failed on the pre-v4 R because of stringsAsFactors differences.

## Purpose
    
- Fixes some issues related to the development release of R 4.0 that sets `stringsAsFactors = FALSE` by default, that were showing up as errors on the CRAN checks (Debian, Fedora, and Solaris platforms).
- Fixes some minor bugs discovered following the milestone v2.0.0 release.
- Moves two data objects to another package, reducing the size of the package.


## Test environments

* local macOS 10.15.3, R 3.6.3
* ubuntu Ubuntu 18.04 LTS and 18.10, R 3.6.3
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* rhub::check_on_debian()
* rhub::check_on_solaris()

## R CMD check results

No ERRORs, NOTEs, or WARNINGs produced.


## Downstream dependencies

This release causes no *new* breaks in other packages.
