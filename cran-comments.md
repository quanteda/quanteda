# Submission notes

*  Major update versus v.3.3.1 -- see NEWS.
*  Numerous bug fixes.
*  Numerous compatibility enhancements with newer versions of some packages (e.g. Matrix).


## Test environments

* local macOS 14.1, R 4.3.2
* Ubuntu 22.04 LTS, R 4.3.2
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* Windows oldrelease via devtools::check_win_oldrelease()

## R CMD check results

An error on the check_*() functions 

* checking re-building of vignette outputs ... [5s] ERROR
Error(s) in re-building vignettes:
--- re-building 'quickstart.Rmd' using rmarkdown

is reported but we cannot figure out what is causing this.

No other warnings or notes are produced.


## Reverse dependency and other package conflicts

According to revdepcheck::revdep_check(), this breaks the. **gofastr** package but we issued a PR fixing this for that package 3 months ago (https://github.com/trinker/gofastr/pull/12) and the package maintainer has yet to accept it.
