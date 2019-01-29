# corpustools

Version: 0.3.3

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 438 marked UTF-8 strings
    ```

# LexisNexisTools

Version: 0.2.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    	...lengths extracted [0.007 secs]
    	...newspapers extracted [0.0073 secs]
    	...dates extracted [0.0094 secs]
    	...authors extracted [0.12 secs]
    	...sections extracted [0.12 secs]
    	...editions extracted [0.12 secs]
    	...headlines extracted [0.13 secs]
    	...dates converted [0.13 secs]
    	...metadata extracted [0.13 secs]
    	...article texts extracted [0.13 secs]
    	...paragraphs extracted [0.14 secs]
    	...superfluous whitespace removed from articles [0.14 secs]
    	...superfluous whitespace removed from paragraphs [0.14 secs]
    Elapsed time: 0.14 secs
    Checking similiarity for 10 articles over 4 dates...
    	...quanteda dfm construced for similarity comparison [0.19 secs].
    	...processing date 2010-01-08: 0 duplicates found [0.20 secs]. 		
    	...processing date 2010-01-09: 0 duplicates found [0.20 secs]. 		Error in validObject(.Object) : 
      invalid class “dfm” object: Dimnames[1] is not a character vector
    Calls: lnt_similarity ... initialize -> callNextMethod -> .nextMethod -> validObject
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      16: initialize(value, ...)
      17: callNextMethod()
      18: .nextMethod(.Object = .Object, ... = ...)
      19: validObject(.Object)
      20: stop(msg, ": ", errors, domain = NA)
      
      Creating LNToutput from input 1 files...
      ══ testthat results  ═══════════════════════════════════════════════════════════════════
      OK: 38 SKIPPED: 0 FAILED: 3
      1. Error: (unknown) (@test-lnt_diff.R#4) 
      2. Error: Test similarity (@test-lnt_similarity.R#8) 
      3. Error: Test similarity warnings and errors (@test-lnt_similarity.R#26) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    LexisNexisTools Version 0.2.1
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    OMP: Warning #96: Cannot form a team with 12 threads, using 2 instead.
    OMP: Hint Consider unsetting KMP_DEVICE_THREAD_LIMIT (KMP_ALL_THREADS), KMP_TEAMS_THREAD_LIMIT, and OMP_THREAD_LIMIT (if any are set).
    Quitting from lines 199-205 (demo.Rmd) 
    Error: processing vignette 'demo.Rmd' failed with diagnostics:
    invalid class "dfm" object: Dimnames[1] is not a character vector
    Execution halted
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
      
      ══ testthat results  ═══════════════════════════════════════════════════════════════════
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

# rJST

Version: 1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rcpp’ ‘RcppProgress’ ‘magrittr’
      All declared Imports should be used.
    ```

# sentometrics

Version: 0.5.6

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

Version: 1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# stm

Version: 1.3.3

## In both

*   R CMD check timed out
    

# stminsights

Version: 0.3.0

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

# tosca

Version: 0.1-3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        data   4.7Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1946 marked UTF-8 strings
    ```

