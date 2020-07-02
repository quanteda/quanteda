# Submission notes

Resubmission - fixes soft dependency on the **formatR** package, now added under `Suggests:`.

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

```
> checking installed package size ... NOTE
    installed size is  5.1Mb
    sub-directories of 1Mb or more:
      R   2.0Mb
```

and: There are several URLs not working on the Windows platform, but in previous correspondence with Uwe Ligges, I was told that this was a CRAN issue due to the JSTOR firewall, and something that CRAN needs to fix (and that I should retain the links despite the warnings).


## Downstream dependencies

This release causes no breaks in other packages.
