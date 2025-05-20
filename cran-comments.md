# Submission notes

## Resubmission

Avoids this problem, by \donttest{}ing the examples for `kwic()`:

```
Check: examples, Result: NOTE
Examples with CPU time > 2.5 times elapsed time
user system elapsed ratio
kwic 0.624 0.466 0.297 3.67
```

## Purpose 

Adds new features and updates.

Addresses two CRAN issues:
* the NOTE about R version dependency caused by our use of `|>`
* clang-UBSAN issues

Checked on: 
* local macOS 15.0.1, R 4.5.0 and devtools::check_mac_release()
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* Windows oldrelease via devtools::check_win_oldrelease()

## R CMD check results

All checks are clean, locally and on GitHub's CI for all the platforms. Neither Valgrind nor Sanitizer raised errors. 

## Reverse dependency and other package conflicts

None, except those noted above.
