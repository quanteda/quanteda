# Submission notes

This is both a fix for issues noted on CRAN checks and a major update to the package.  The major update is to bring new functionalities to quanteda, make the code more efficient, and rationalise the code through new deprecations and removals.

The fixes to existing CRAN issues include:

*  We now compile the TBB library directly into the package for parallelism, rather than relying on RcppParallel. Relying on the the TBB implementation from that package led to complications with UBSAN and "Additional Issues" on some platforms that we could not fix within that framework.
*  Fixes problems notified by CRAN concerning UseMethod no longer forwarding local variables from the generic.

Major changes include:

*  A major update versus v.3.3.1, with many new features and improvements -- see NEWS.
*  Numerous compatibility enhancements with newer versions of some packages (e.g. Matrix).
*  Numerous bug fixes.

## Test environments

* local macOS 14.2.1, R 4.3.2
* Ubuntu 22.04 LTS, R 4.3.2
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* Windows oldrelease via devtools::check_win_oldrelease()

## R CMD check results

All checks are clean, locally and on GitHub's CI for multiple platforms, with the exception of this NOTE:

* checking compilation flags in Makevars ... NOTE
Package has both ‘src/Makevars.in’ and ‘src/Makevars’.
Installation with --no-configure' is unlikely to work.  If you intended
‘src/Makevars’ to be used on Windows, rename it to ‘src/Makevars.win’
otherwise remove it.  If ‘configure’ created ‘src/Makevars’, you need a
‘cleanup’ script.

## Reverse dependency and other package conflicts

According to revdepcheck::revdep_check(), this breaks the **gofastr** package because that package calls functions that we first deprecated in version 3.3 and have now made defunct.  We issued a pull request fixing this for that package over four months ago (https://github.com/trinker/gofastr/pull/12) and the package maintainer has yet to accept it.
