# Submission notes

This is a major new release that completes the modularisation begin in quanteda v2.  It also:
    - improves the consistency of many functions
    - adds functionality through new functions
    - removes many deprecations introduced in v2
    - reduces the package dependencies

All of this is designed to improve stability and maintainability for the future.

As a major release, however, this introduces complications for the CRAN checks.

1. This breaks quanteda.textstats, although we have simultaneously submitted a new version of that package that will not be broken by this release.

2. This breaks two other packages, although we have fixed both for the authors.
    - **collector**, although we fixed this for the package author in https://github.com/davidski/collector/pull/5, submitted December 2020
    - and **politeness**, although we fixed this in https://github.com/myeomans/politeness/pull/3, submitted 5 March 2020 and merged by the package author.



## Test environments

* local macOS 10.15.7, R 4.0.4
* Ubuntu 20.04 LTS, R 4.0.4
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* Windows oldrelease via devtools::check_win_oldrelease()


## R CMD check results

No ERRORs, NOTEs, or WARNINGs produced, _except_ those notes above.


## Downstream dependencies

This release causes no breaks in other packages, as checked via `revdepcheck::revdepcheck()`.
