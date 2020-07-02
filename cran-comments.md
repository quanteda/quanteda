# Submission notes

Resubmission - fixes soft dependency on the **formatR** package, now added under `Suggests:`, and avoids having to source external data from a `url()` call in the Quickstart Guide vignette that failed on the CRAN Windows platform checks.

## Purpose
    
- Numerous improvements and some bug fixes.
- Fixes a noLD issue (on Solaris) noted on 25 June by Brian Ripley by email.
- Fixes some errors on the CRAN results, related to the missing soft dependency on the **formatR** package.

## Test environments

* local macOS 10.15.5, R 4.0.1
* Ubuntu 18.04 LTS and 18.10, R 4.0.2
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()

## R CMD check results

No ERRORs, NOTEs, or WARNINGs produced, _except_:

There are WARNINGS from several URLs referencing JSTOR articles that are not working on the Windows platform, but in correspondence with Uwe Ligges, he told me that this was a CRAN issue due to the JSTOR firewall, and something that I should ignore.


## Downstream dependencies

This release causes no breaks in other packages.
