# Submission notes

## Purpose
    
- Fixes a breaking test caused by changes to the **tidytext** package.  
- Implements a few minor bug fixes and stability enhancements.  
- Updates `textstat_simil()` and `textstat_dist()`, adding efficiency gains, additional options, and more strictly enforced methods.

## Test environments

* local macOS 10.14.5, R 3.6.0
* ubuntu Ubuntu 18.04 LTS and 18.10, R 3.6.0
* Windows Server 2012 R2 x64 (build 9600), R 3.6.0 (on Appveyor)
* local Windows 10, R 3.6.0
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

No errors, warnings, or notes were caused in other packages, using `revdepcheck::revdep_check()` to confirm, EXCEPT:

New format changes in `textstat_simil()` caused an error in **LexisNexisTools**, but we fixed this in that package in https://github.com/JBGruber/LexisNexisTools/pull/9, and the author will soon resubmit this update to CRAN.
