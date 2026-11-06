# Submission notes

## Resubmission

This is a patch release (4.5.1) that fixes the test failures reported on the
r-devel-linux-x86_64-fedora-clang and r-devel-linux-x86_64-fedora-gcc CRAN check
flavours for version 4.5.0.

The failures arose because tm 0.7-20 (published 2026-09-30) moved **NLP** from
`Depends` to `Imports`, so `library(tm)` no longer attaches **NLP**. Two tests in
`tests/testthat/test-corpus.R` called `detach("package:NLP")` unconditionally and
therefore errored. The tests now detach **NLP** only if it is attached. There are
no changes to the package code.

## R CMD check results

`tests/testthat/test-corpus.R` passes locally with tm 0.7-20 installed (0 failures;
2 failures before the fix).

TODO before submission: run `devtools::check()`, `check_win_devel()` and
`check_mac_release()`, and record the results here.

## Reverse dependency and other package conflicts

Only a test file has changed, so the package behaviour is identical to 4.5.0 and
no new reverse dependency problems are expected.
