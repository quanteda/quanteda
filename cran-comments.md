# Submission notes

## Purpose
    
- Minor update to fix tests breaking on Solaris.

## Test environments

* local macOS 10.15.5, R 4.0.2
* Ubuntu 18.04 LTS and 18.10, R 4.0.2
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()

## R CMD check results

No ERRORs, NOTEs, or WARNINGs produced, _except_:

There are WARNINGS from several URLs referencing JSTOR articles that are not working on the Windows platform, but in correspondence with Uwe Ligges, he told me that this was a CRAN issue due to the JSTOR firewall, and something that I should ignore.


## Downstream dependencies

This release causes no breaks in other packages.
