# Submission notes

## Purpose

Feature updates and bug fixes.  See NEWS.md for details.

## R CMD check results

Checked on:
* local macOS 26.5.2, R 4.6.1 and devtools::check_mac_release()
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* Windows oldrelease via devtools::check_win_oldrelease()

All checks are clean, locally and on GitHub's CI for all the platforms.

## Reverse dependency and other package conflicts

We checked all reverse dependencies using `revdepcheck::revdep_check()`.

There is a single clash with the package **sentopics** but we have issue a pull request fixing this. https://github.com/odelmarcelle/sentopics/pull/6

When that has been merged and **sentopics** resubmitted, we will resubmit **quanteda** 4.5.0 to CRAN.
