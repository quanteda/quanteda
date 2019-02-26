# Submission notes

## Purpose
    
- Fixes error on some Linux implementations caused by a **stringi** package update.

## Test environments

* local macOS 10.14.3, R 3.5.2
* ubuntu Ubuntu 18.04 LTS and 18.10, R 3.5.2
* Windows Server 2012 R2 x64 (build 9600), R 3.5.2 (on Appveyor)
* local Windows 10, R 3.5.2
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

In the current (but not development) version tests, we also see this:
```
* checking installed package size ... NOTE
  installed size is  6.6Mb
  sub-directories of 1Mb or more:
    R      1.0Mb
    data   1.3Mb
    libs   3.3Mb
```

## Downstream dependencies

No errors, warnings, or notes were caused in other packages, using `revdepcheck::revdep_check()` to confirm.
