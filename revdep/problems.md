# corpustools

Version: 0.3.3

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 438 marked UTF-8 strings
    ```

# newsmap

Version: 0.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringi’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1491 marked UTF-8 strings
    ```

# phrasemachine

Version: 1.1.2

## In both

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: phrasemachine
    > ### Title: POS tag and extract phrases from a collection of documents
    > ### Aliases: phrasemachine
    > 
    > ### ** Examples
    > 
    > phrasemachine("Hello there my red good cat.")
    Currently tagging document 1 of 1 
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    Warning in system("/usr/libexec/java_home", intern = TRUE) :
      running command '/usr/libexec/java_home' had status 1
    Error: .onLoad failed in loadNamespace() for 'rJava', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/kbenoit/Dropbox (Personal)/GitHub/quanteda/quanteda/revdep/library.noindex/phrasemachine/rJava/libs/rJava.so':
      dlopen(/Users/kbenoit/Dropbox (Personal)/GitHub/quanteda/quanteda/revdep/library.noindex/phrasemachine/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
      Referenced from: /Users/kbenoit/Dropbox (Personal)/GitHub/quanteda/quanteda/revdep/library.noindex/phrasemachine/rJava/libs/rJava.so
      Reason: image not found
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      7: asNamespace(ns)
      8: getNamespace(ns)
      9: tryCatch(loadNamespace(name), error = function(e) stop(e))
      10: tryCatchList(expr, classes, parentenv, handlers)
      11: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      12: value[[3L]](cond)
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════════════════════════════════════════
      OK: 10 SKIPPED: 0 FAILED: 3
      1. Error: See if tagging five documents works, and if coarsening works (@test_POS_tag_documents.R#10) 
      2. Error: See if extractor works (@test_extract_phrases.R#10) 
      3. Error: extract right spans (@test_phrasemachine.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    Quitting from lines 116-124 (getting_started_with_phrasemachine.Rmd) 
    Error: processing vignette 'getting_started_with_phrasemachine.Rmd' failed with diagnostics:
    .onLoad failed in loadNamespace() for 'rJava', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/kbenoit/Dropbox (Personal)/GitHub/quanteda/quanteda/revdep/library.noindex/phrasemachine/rJava/libs/rJava.so':
      dlopen(/Users/kbenoit/Dropbox (Personal)/GitHub/quanteda/quanteda/revdep/library.noindex/phrasemachine/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
      Referenced from: /Users/kbenoit/Dropbox (Personal)/GitHub/quanteda/quanteda/revdep/library.noindex/phrasemachine/rJava/libs/rJava.so
      Reason: image not found
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

# readtext

Version: 0.71

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked Latin-1 string
      Note: found 1 marked UTF-8 string
      Note: found 7 strings marked as "bytes"
    ```

# sentometrics

Version: 0.5.1

## In both

*   checking whether package ‘sentometrics’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/kbenoit/Dropbox (Personal)/GitHub/quanteda/quanteda/revdep/checks.noindex/sentometrics/new/sentometrics.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘sentometrics’ ...
** package ‘sentometrics’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/kbenoit/Dropbox (Personal)/GitHub/quanteda/quanteda/revdep/library.noindex/sentometrics/Rcpp/include" -I"/Users/kbenoit/Dropbox (Personal)/GitHub/quanteda/quanteda/revdep/library.noindex/sentometrics/RcppArmadillo/include" -I"/Users/kbenoit/Dropbox (Personal)/GitHub/quanteda/quanteda/revdep/library.noindex/sentometrics/RcppParallel/include" -I/usr/local/include  -fopenmp -fPIC  -Wall -g -O2 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘sentometrics’
* removing ‘/Users/kbenoit/Dropbox (Personal)/GitHub/quanteda/quanteda/revdep/checks.noindex/sentometrics/new/sentometrics.Rcheck/sentometrics’

```
### CRAN

```
* installing *source* package ‘sentometrics’ ...
** package ‘sentometrics’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/kbenoit/Dropbox (Personal)/GitHub/quanteda/quanteda/revdep/library.noindex/sentometrics/Rcpp/include" -I"/Users/kbenoit/Dropbox (Personal)/GitHub/quanteda/quanteda/revdep/library.noindex/sentometrics/RcppArmadillo/include" -I"/Users/kbenoit/Dropbox (Personal)/GitHub/quanteda/quanteda/revdep/library.noindex/sentometrics/RcppParallel/include" -I/usr/local/include  -fopenmp -fPIC  -Wall -g -O2 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘sentometrics’
* removing ‘/Users/kbenoit/Dropbox (Personal)/GitHub/quanteda/quanteda/revdep/checks.noindex/sentometrics/old/sentometrics.Rcheck/sentometrics’

```
# spacyr

Version: 0.9.91

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# stminsights

Version: 0.2.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘huge’ ‘readr’ ‘scales’ ‘shinyjs’
      All declared Imports should be used.
    ```

# stopwords

Version: 0.9.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 11790 marked UTF-8 strings
    ```

# tidytext

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

