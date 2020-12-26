# collector

<details>

* Version: 0.1.3
* GitHub: https://github.com/davidski/collector
* Source code: https://github.com/cran/collector
* Date/Publication: 2020-02-18 00:10:02 UTC
* Number of recursive dependencies: 165

Run `revdep_details(, "collector")` for more info

</details>

## Newly broken

*   checking whether package ‘collector’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/kbenoit/Dropbox (Personal)/GitHub/quanteda/quanteda/revdep/checks.noindex/collector/new/collector.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘collector’ ...
** package ‘collector’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘textstat_readability’ is not exported by 'namespace:quanteda'
Execution halted
ERROR: lazy loading failed for package ‘collector’
* removing ‘/Users/kbenoit/Dropbox (Personal)/GitHub/quanteda/quanteda/revdep/checks.noindex/collector/new/collector.Rcheck/collector’

```
### CRAN

```
* installing *source* package ‘collector’ ...
** package ‘collector’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (collector)

```
# LSX

<details>

* Version: 0.9.5
* GitHub: NA
* Source code: https://github.com/cran/LSX
* Date/Publication: 2020-11-20 06:40:02 UTC
* Number of recursive dependencies: 112

Run `revdep_details(, "LSX")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [90m 1. [39m├─utils::head(...) [90mtest-utils.R:4:0[39m
      [90m 2. [39m└─LSX::char_keyness(toks_test, "america*", min_count = 1, p = 0.05)
      [90m 3. [39m  └─LSX::char_context(...)
      [90m 4. [39m    └─LSX::textstat_context(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [33mError[39m (test-as.textmodel.R:5:1): (code run outside of `test_that()`)
      [33mError[39m (test-textmodel.R:9:1): (code run outside of `test_that()`)
      [33mError[39m (test-textstat.R:7:5): textstat_context works
      [33mError[39m (test-textstat.R:18:5): char_context removes multi-word target
      [33mError[39m (test-utils.R:4:1): (code run outside of `test_that()`)
      
      [ [33mFAIL[39m 5 | [35mWARN[39m 0 | [34mSKIP[39m 0 | [32mPASS[39m 7 ]
      Error: Test failures
      Execution halted
    ```

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'textmodel_lss.Rd':
      ‘[quanteda:textstat_simil]{quanteda::textstat_simil()}’
    
    Missing link or links in documentation object 'textstat_context.Rd':
      ‘textstat_keyness’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    textstat_context: no visible global function definition for
      ‘textstat_keyness’
    Undefined global functions or variables:
      textstat_keyness
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘e1071’ ‘grDevices’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2096 marked UTF-8 strings
    ```

# newsmap

<details>

* Version: 0.7.2
* GitHub: https://github.com/koheiw/newsmap
* Source code: https://github.com/cran/newsmap
* Date/Publication: 2020-08-03 23:00:03 UTC
* Number of recursive dependencies: 89

Run `revdep_details(, "newsmap")` for more info

</details>

## Newly broken

*   checking R code for possible problems ... NOTE
    ```
    afe: no visible global function definition for ‘textstat_entropy’
    Undefined global functions or variables:
      textstat_entropy
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringi’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4485 marked UTF-8 strings
    ```

# quanteda.textmodels

<details>

* Version: 0.9.2
* GitHub: https://github.com/quanteda/quanteda.textmodels
* Source code: https://github.com/cran/quanteda.textmodels
* Date/Publication: 2020-12-11 11:10:02 UTC
* Number of recursive dependencies: 119

Run `revdep_details(, "quanteda.textmodels")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 35707 marked UTF-8 strings
    ```

