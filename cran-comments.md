# Submission notes

*  Fixes problems notified by CRAN concerning UseMethod no longer forwarding local variables from the generic.
*  Major update versus v.3.3.1, with many new features and improvements -- see NEWS.
*  Numerous bug fixes.
*  Numerous compatibility enhancements with newer versions of some packages (e.g. Matrix).

## Test environments

* local macOS 14.2.1, R 4.3.2
* Ubuntu 22.04 LTS, R 4.3.2
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* Windows oldrelease via devtools::check_win_oldrelease()

## R CMD check results

All checks are clean, locally and on GitHub's CI for multiple platforms.

## Reverse dependency and other package conflicts

According to revdepcheck::revdep_check(), this breaks the **gofastr** package because that package calls functions that we first deprecated in version 3.3 and have now made defunct.  We issued a pull request fixing this for that package over four months ago (https://github.com/trinker/gofastr/pull/12) and the package maintainer has yet to accept it.
