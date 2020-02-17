# Submission notes

## Purpose
    
- A major version update with many new features and changes, documented in NEWS.md.
- Fixes a CRAN check failure caused by documented objects ... not in the function signatures for two functions, causing CRAN errors in v1.5.2

## Test environments

* local macOS 10.15.3, R 3.6.2
* ubuntu Ubuntu 18.04 LTS and 18.10, R 3.6.2
* Windows Server 2012 R2 x64 (build 9600), R 3.6.2 (on Appveyor)
* devtools::check_win_release()
* rhub::check_on_solaris()

## R CMD check results

No ERRORs, NOTEs, or WARNINGs produced.


## Downstream dependencies

The **RNewsflow** package... had a problem in executing an example but I fixed this in a pull request to that package (https://github.com/kasperwelbers/RNewsflow/pull/4) that has now been merged.  The maintainer Kasper Welbers (kasperwelbers@gmail.com) is planning to refresh the CRAN version today.
