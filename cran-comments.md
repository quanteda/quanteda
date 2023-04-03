# Submission notes

* Fixes a noSuggests caused by archived (and a week later restored) Suggested kage quanteda.textmodels

* Removes the C++ requirement.

* Implements a new experimental tokenizer.

## Test environments

* local macOS 13.2.1, R 4.2.3
* Ubuntu 22.04 LTS, R 4.2.3
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* Windows oldrelease via devtools::check_win_oldrelease()

## R CMD check results

No ERRORs, NOTEs, or WARNINGs produced.

## Reverse dependency and other package conflicts

None, according to revdepcheck::revdep_check().
