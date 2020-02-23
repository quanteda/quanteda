# Submission notes

## Purpose
    
- A major version update with many new features and changes, documented in NEWS.md.
- Fixes a CRAN check failure caused by documented objects `...` not in the function signatures for two functions, causing CRAN errors in v1.5.2.

## Test environments

* local macOS 10.15.3, R 3.6.2
* ubuntu Ubuntu 18.04 LTS and 18.10, R 3.6.2
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* rhub::check_on_solaris()

## R CMD check results

No ERRORs, NOTEs, or WARNINGs produced, except:

```
Suggests or Enhances not in mainstream repositories:
  quanteda.textmodels
```

but we submitted quanteda.textmodels as a new package to CRAN on 18 February so this will be fixed once that package is accepted.

## Downstream dependencies

There are three reverse imports that our new version causes (minor) problems for, but we have not only notified all of these package maintainers over two months ago, but also fixed the problems via pull requests.  The package maintainers only need to resubmit their packages to CRAN (from their GitHub master branches) to fix the problems that our new version will cause.

**tosca**:  This package errors on a test but I notified the maintainers and actuall fixed this code in a pull request that was merged into the **tosca** master branch on 21 December 2019: https://github.com/Docma-TU/tosca/commit/dae6ed5bb7457a65693d130dfeb6388b75ae27b4 (21 December 2019).  They simply need to resubmit the updated (and passing, on GitHub) package to CRAN.

**tidytext**:  We fixed the breaking changes in a pull request accepted on 7 December 2019 (https://github.com/juliasilge/tidytext/pull/160), and this is fixed now in the GitHub master.  They simply need to resubmit the updated (and passing, on GitHub) package to CRAN.

**preText**:  The split of some of the functions previously in **quanteda** into a new **quanteda.textmodels** package meant that **preText** needed its namespace references to these functions updated.  I've issued a pull request (https://github.com/matthewjdenny/preText/pull/9) to fix this, and suggested that the maintainer refresh this package on CRAN as soon as **quanteda.textmodels** is accepted and published.
