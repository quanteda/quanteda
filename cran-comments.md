# Re-submission notes

Fixed:

- A failing test caused by the ever-shifting behaviour of **Matrix** and the devel R on r-devel-linux-x86_64-debian-clang and r-devel-linux-x86_64-debian-gcc.

- An Undeclared package ‘quanteda.textstats’ in Rd xrefs.

- An installation failure on r-devel-linux-x86_64-fedora-gcc due to searching for TBB in all the wrong places.


## Test environments

* local macOS 14.4.1, R 4.3.3
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* Windows oldrelease via devtools::check_win_oldrelease()

## R CMD check results

All checks are clean, locally and on GitHub's CI for multiple platforms.

## Reverse dependency and other package conflicts

None, except those noted above.
