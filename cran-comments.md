# Submission notes

## Resubmission

We worked with the authors of the **sentopics** package to fix a breaking change that affected that package. They have resubmitted their package and we have verified that our new version works with it now.

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

There was a single previous clash with the package **sentopics** but we have fixed that, and the **sentopics** authors have published the fixed version now on CRAN.
