# Submission notes

## Purpose 

Adds new features and updates.

Fixes a problem in the data_corpus_inaugural data object.

Checked on: 
* local macOS 15.0.1, R 4.5.0 and devtools::check_mac_release()
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* Windows oldrelease via devtools::check_win_oldrelease()

## R CMD check results

All checks are clean, locally and on GitHub's CI for all the platforms. Neither Valgrind nor Sanitizer raised errors. 

## Reverse dependency and other package conflicts

None, except those noted above.
