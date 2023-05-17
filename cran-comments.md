# Submission notes

*  Fixed a potential crash when calling `tokens_compound()` with patterns containing paddings (#2254).
*  Updated for compatibility with (forthcoming) Matrix 1.5.5 handling of dimnames() 
for empty dimensions.
*  restores `readtext` object class method extensions, to work better with the **readtext** package.
*  Removes some unused internal methods, such as `docvars.kwic()` that were not exported despite matching exported generics.


## Test environments

* local macOS 13.2.1, R 4.3.0
* Ubuntu 22.04 LTS, R 4.3.0
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* Windows oldrelease via devtools::check_win_oldrelease()

## R CMD check results

No ERRORs, NOTEs, or WARNINGs produced.

## Reverse dependency and other package conflicts

None, according to revdepcheck::revdep_check().
