# Submission notes

## Purpose
    
- Implements a request in an email from Brian Ripley (21 Jan 2019) that we ensure a two-thread limit for OpenMP usage (by setting OMP_THREAD_LIMIT=2),
caused by unit tests whose results differed on Debian versus other platforms.  
- Implements bug fixes and stability enhancements.
- Adds several new features documented in NEWS.md.

## Test environments

* local macOS 10.14.3, R 3.5.2
* ubuntu Ubuntu 18.04 LTS and 18.04 LTS, R 3.5.2
* Windows Server 2012 R2 x64 (build 9600), R 3.5.2 (on Appveyor)
* local Windows 10, R 3.5.2
* win-builder (devel and release)

## R CMD check results

We're run this through every checker possible: local builds, win-builder, r-hub, and revdep_check.  It's clean, except:

### Note on UBSAN issues

Our package has some recurring UBSAN issues related to the Intel TBB (Threading Building Blocks) library built into **RcppParallel**.  However, these are harmless and should not block acceptance of our package.  

We have been in a wide discussion with the RcppParallel development team (see https://github.com/RcppCore/RcppParallel/issues/36) but they have identified the problem as an object call in TBB.  This seems to have no consequences for stability in packages that use these functions.  One of the RcppParallel developers, Kevin Ushey (kevinushey@gmail.com) has confirmed this.

We are far from the only package to be affected by these (harmless) compiler warnings, for instance the following are also affected:

* RcppMeCab - https://www.stats.ox.ac.uk/pub/bdr/gcc8/RcppMeCab.out
* gaston — https://www.stats.ox.ac.uk/pub/bdr/memtests/gcc-UBSAN/gaston/build_vignettes.log
* RcppParallel - https://www.stats.ox.ac.uk/pub/bdr/memtests/gcc-UBSAN/RcppParallel/RcppParallel-Ex.Rout

We hope that this will not block the updating of our package, and also expect that as TBB improves (from Intel) and this filters into RcppParallel, these warnings will eventually go away.


### ERRORs or WARNINGs

None, although see above re: UBSAN.

### NOTES

None.

## Downstream dependencies

No errors, warnings, or notes were caused in other packages, using `revdepcheck::revdep_check()` to confirm.
