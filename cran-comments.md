# Submission notes

- Fixes breaking tests and examples caused by changes to the stringi package
- Fixes errors on Solaris introduced in quanteda 2.1.0/1
- Minor performance improvements

## Test environments

* local macOS 10.15.6, R 4.0.2
* Ubuntu 18.04 LTS and 18.10, R 4.0.2
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()

## R CMD check results

No ERRORs, NOTEs, or WARNINGs produced, _except_:

> checking installed package size ... NOTE
    installed size is  5.1Mb
    sub-directories of 1Mb or more:
      R   2.0Mb

There are WARNINGS from several URLs referencing JSTOR articles that are not working on the Windows platform, but in correspondence with Uwe Ligges, he told me that this was a CRAN issue due to the JSTOR firewall, and something that I should ignore.


## Downstream dependencies

This release causes no breaks in other packages, as checked via `revdepcheck::revdepcheck()`.
