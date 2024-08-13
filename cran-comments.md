# Re-submission notes

Fixed:

- A failing test caused by C++ code related to `fcm()` and how tokens objects are re-indexed.

- An undeclared package ‘quanteda.textstats’ in Rd xrefs.

## Test environments

* local macOS 14.4.1, R 4.3.3
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* Windows oldrelease via devtools::check_win_oldrelease()

## R CMD check results

All checks are clean, locally and on GitHub's CI for multiple platforms.

## Reverse dependency and other package conflicts

None, except those noted above.
