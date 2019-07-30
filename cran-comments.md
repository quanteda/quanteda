# Submission notes

## Purpose
    
- Fixes a few bugs from the 1.5.0 release.

## Test environments

* local macOS 10.14.5, R 3.6.1
* ubuntu Ubuntu 18.04 LTS and 18.10, R 3.6.1
* Windows Server 2012 R2 x64 (build 9600), R 3.6.1 (on Appveyor)
* local Windows 10, R 3.6.1
* win-builder (devel and release)

## R CMD check results

### ERRORs or WARNINGs

None.

### NOTES

Some of the URLs linking to JSTOR articles in `textstat_readability.Rd` generate these:
```
Found the following (possibly) invalid URLs:
  URL: https://www.jstor.org/stable/1433978
    From: man/textstat_readability.Rd
    Status: 403
    Message: Forbidden
```
but in correspondence with Uwe Ligges, he told me that this is due more to a CRAN configuration issue than something that should be fixed in the package, so I am leaving these.

In some tests we also see a NOTE that the installed size is approx. 6.6Mb.

## Downstream dependencies

No errors, warnings, or notes were caused in other packages, using `revdepcheck::revdep_check()` to confirm, *except*:

The **RNewsflow** package had a problem in executing an example but I fixed this in a pull request to that package (https://github.com/kasperwelbers/RNewsflow/pull/4) that has now been merged.  The maintainer Kasper Welbers (kasperwelbers@gmail.com) is planning to refresh the CRAN version today.
