# Submission notes

## RESUBMISSION

- Fixes a test based on `sample()` that failed in the devel versions of R because of changes made to the random number generator.

## Purpose
    
- Fixes some "unable to re-encode" errors from r-devel-linux-x86_64-debian-clang caused by special hyphens in some of our documentation.
- Fixes some compiler warnings on r-patched-solaris-x86, noted by Brian Ripley in email.
- Implements a few minor bug fixes and stability enhancements.

## Test environments

* local macOS 10.14.4, R 3.5.3
* ubuntu Ubuntu 18.04 LTS and 18.10, R 3.5.3
* Windows Server 2012 R2 x64 (build 9600), R 3.5.2 (on Appveyor)
* local Windows 10, R 3.5.3
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

No errors, warnings, or notes were caused in other packages, using `revdepcheck::revdep_check()` to confirm.
