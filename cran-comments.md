# Resubmission

We have done our best to address the UBSAN issues flagged in our recent submission (and showing on the current CRAN version).  This includes working with the Rcpp team - see https://github.com/RcppCore/Rcpp/issues/1326.  This bug was very hard to reproduce and the Rcpp team even suspected that it might be a bug in the sanitizer, but by changing a variable type (https://github.com/quanteda/quanteda/pull/2423/files) we seem to be avoiding the sanitizer exceptions now, at least on the setup we are able to test.

# Submission notes

Fixed:
- Warnings and additional issues noted with 4.0.2.

Checked on: 
* local macOS 14.4.1, R 4.4.1 and devtools::check_mac_release()
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* Windows oldrelease via devtools::check_win_oldrelease()
* Linux devel (2024-08-16 r87025) compiled with clang, Address Sanitizer and Undefined Behavior Sanitizer.
* Linux devel (2024-08-16 r87025) compiled with gcc, valgrind level 2 instrumentation.

We used the Docker container available at https://github.com/wch/r-debug for tests on Linux devel. 

## R CMD check results

All checks are clean, locally and on GitHub's CI for all the platforms. Neither Valgrind nor Sanitizer raised errors. 

## Reverse dependency and other package conflicts

None, except those noted above.
