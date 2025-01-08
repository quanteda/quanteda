# Submission notes

Fixed:
- Bugs in 4.1.0
- Improved output messages
- Improved minor behaviours of some functions

Checked on: 
* local macOS 15.0.1, R 4.4.2 and devtools::check_mac_release()
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
