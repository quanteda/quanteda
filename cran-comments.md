# Submission notes

## Purpose
    
- Some bug fixes.
- Added a few minor features.
- Added future compatibility in anticipation of the migration to v2 corpus object classes.

## Test environments

* local macOS 10.14.6, R 3.6.1
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
  URL: https://www.jstor.org/stable/pdf/1473669
    From: man/textstat_readability.Rd
    Status: 403
    Message: Forbidden
  URL: https://www.jstor.org/stable/pdf/41384251
    From: man/textstat_readability.Rd
    Status: 403
    Message: Forbidden
  URL: https://www.jstor.org/stable/pdf/747086
    From: man/textstat_readability.Rd
    Status: 403
    Message: Forbidden
```
but in correspondence with Uwe Ligges, he told me that this is due more to a CRAN configuration issue than something that should be fixed in the package, so I am leaving these.

In some tests we also see a NOTE that the installed size is approx. 6.6Mb.

## Downstream dependencies

No errors, warnings, or notes were caused in other packages.
