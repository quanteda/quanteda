# Re-submission notes

Fixed:

- Breaking documentation link in two .Rd files in the package **tosca**: fixed on the **quanteda** side.

- **RNewsflow** - Fised on CRAN, based on our PR (https://github.com/kasperwelbers/RNewsflow/pull/9).

Informed and pending a fix:

- **tidytext**: We have not only informed the package maintainer that their use in a vignette of a function deprecated in v3 (released in 2021) is now defunct, but also we issued a PR to fix this (https://github.com/juliasilge/tidytext/pull/242).

- **gofastr**: That package calls functions that we first deprecated in version 3.3 and have now made defunct.  We issued a pull request fixing this for that package nearly six months ago (https://github.com/trinker/gofastr/pull/12) and the package maintainer has yet to accept it.


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

* local macOS 14.2.1, R 4.3.3
* Ubuntu 22.04 LTS, R 4.3.3
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* Windows oldrelease via devtools::check_win_oldrelease()

## R CMD check results

All checks are clean, locally and on GitHub's CI for multiple platforms.

## Reverse dependency and other package conflicts

None, except those noted above.
