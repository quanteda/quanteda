# clustRcompaR

Version: 0.2.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > library(quanteda)
    Package version: 1.4.0
    Parallel computing: 2 of 12 threads used.
    See https://quanteda.io for tutorials and examples.
    
    Attaching package: ‘quanteda’
    
    The following object is masked from ‘package:utils’:
    
        View
    
    > 
    > d <- inaugural_addresses
    > d <- mutate(d, century = ifelse(Year < 1800, "17th",
    +                                 ifelse(Year >= 1800 & Year < 1900, "18th",
    +                                        ifelse(Year >= 1900 & Year < 2000, "19th", "20th"))))
    > 
    > three_clusters <- cluster(d, century, n_clusters = 3)
    Error: 'metadoc<-' is not an exported object from 'namespace:quanteda'
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [31m──[39m [31m1. Error: (unknown) (@test_cases.R#9) [39m [31m───────────────────────────────────────────────────────────────[39m
      'metadoc<-' is not an exported object from 'namespace:quanteda'
      1: cluster(d, n_clusters = 3) at testthat/test_cases.R:9
      2: assemble_corpus(data, stopwords = all_stopwords, remove_twitter = remove_twitter)
      3: quanteda::`metadoc<-`
      4: getExportedValue(pkg, name)
      5: stop(gettextf("'%s' is not an exported object from 'namespace:%s'", name, getNamespaceName(ns)), call. = FALSE, 
             domain = NA)
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════════
      OK: 0 SKIPPED: 0 FAILED: 1
      1. Error: (unknown) (@test_cases.R#9) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 40-52 (introduction-to-clustRcompaR.Rmd) 
    Error: processing vignette 'introduction-to-clustRcompaR.Rmd' failed with diagnostics:
    'metadoc<-' is not an exported object from 'namespace:quanteda'
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘quanteda::metadoc’
    ```

# corpustools

Version: 0.3.3

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 438 marked UTF-8 strings
    ```

# LexisNexisTools

Version: 0.2.2

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      	...processing date 2010-01-09: 0 duplicates found [0.025 secs]. 		
      	...processing date 2010-01-10: 0 duplicates found [0.041 secs]. 		
      	...processing date 2010-01-11: 9 duplicates found [1.89 secs]. 		
      Threshold = 0.99; 4 days processed; 4 duplicates found; in 1.89 secsChecking similiarity for 10 articles over 3 dates...
      	...quanteda dfm construced for similarity comparison [0.027 secs].
      	...processing date 2010-01-08: 0 duplicates found [0.027 secs]. 		
      	...processing date 2010-01-10: 0 duplicates found [0.043 secs]. 		
      	...processing date 2010-01-11: 9 duplicates found [1.86 secs]. 		
      Threshold = 0.99; 3 days processed; 4 duplicates found; in 1.86 secsCreating LNToutput from input 1 files...
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════════
      OK: 48 SKIPPED: 0 FAILED: 1
      1. Error: Convert LNToutput to quanteda (@test-lnt_convert.R#19) 
      
      Error: testthat unit tests failed
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
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════════
      OK: 8 SKIPPED: 0 FAILED: 3
      1. Error: See if tagging five documents works, and if coarsening works (@test_POS_tag_documents.R#7) 
      2. Error: See if extractor works (@test_extract_phrases.R#7) 
      3. Error: extract right spans (@test_phrasemachine.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 82-91 (getting_started_with_phrasemachine.Rmd) 
    Error: processing vignette 'getting_started_with_phrasemachine.Rmd' failed with diagnostics:
    unused argument (alist())
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

# preText

Version: 0.6.2

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
                          Arthur Spirling, NYU
      Type vignette('getting_started_with_preText') to get started.
      Development website: https://github.com/matthewjdenny/preText
      > 
      > test_check("preText")
      [31m──[39m [31m1. Error: Small example works (@test_factorial_preprocessing.R#6) [39m [31m───────────────────────────────────[39m
      unused argument (alist())
      1: documents[1:10, ] at testthat/test_factorial_preprocessing.R:6
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════════
      OK: 0 SKIPPED: 0 FAILED: 1
      1. Error: Small example works (@test_factorial_preprocessing.R#6) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 60-69 (getting_started_with_preText.Rmd) 
    Error: processing vignette 'getting_started_with_preText.Rmd' failed with diagnostics:
    unused argument (alist())
    Execution halted
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

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘sentometrics-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add_features
    > ### Title: Add feature columns to a (sento)corpus object
    > ### Aliases: add_features
    > 
    > ### ** Examples
    > 
    > data("usnews", package = "sentometrics")
    > 
    > set.seed(505)
    > 
    > # construct a corpus and add (a) feature(s) to it
    > corpus <- quanteda::corpus_sample(sento_corpus(corpusdf = usnews), 500)
    Warning: metacorpus argument is not used in sento_corpus()
    Warning in corp$tokens <- NULL : Coercing LHS to a list
    Error in corpus_sample.default(sento_corpus(corpusdf = usnews), 500) : 
      corpus_sample() only works on corpus objects.
    Calls: <Anonymous> -> corpus_sample.default
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════════
      OK: 15 SKIPPED: 0 FAILED: 10
      1.  Error: (unknown) (@test_aggregation.R#11) 
      2.  Error: (unknown) (@test_attribution.R#11) 
      3.  Failure: Corpus building works and fails when appropriate (@test_corpus_building.R#15) 
      4.  Error: Corpus building works and fails when appropriate (@test_corpus_building.R#17) 
      5.  Error: Conversion to sentocorpus from quanteda corpus (@test_corpus_building.R#26) 
      6.  Error: (unknown) (@test_corpus_building.R#35) 
      7.  Error: (unknown) (@test_measures_manipulation.R#11) 
      8.  Error: (unknown) (@test_methods_sentomeasures.R#11) 
      9.  Error: (unknown) (@test_model_specification.R#11) 
      10. Error: (unknown) (@test_sentiment_computation.R#21) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘MCS’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4436 marked UTF-8 strings
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
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

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      3: purrr::compact(quanteda::metacorpus(x))
      4: discard(.x, function(x) is_empty(.f(x)))
      5: probe(.x, .p, ...)
      6: map_lgl(.x, .p, ...)
      7: quanteda::metacorpus
      8: getExportedValue(pkg, name)
      9: stop(gettextf("'%s' is not an exported object from 'namespace:%s'", name, getNamespaceName(ns)), call. = FALSE, 
             domain = NA)
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════════
      OK: 276 SKIPPED: 0 FAILED: 1
      1. Error: Can glance a corpus from quanteda using accessor functions (@test-corpus-tidiers.R#62) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    Missing or unexported object: ‘quanteda::metacorpus’
    ```

# tosca

Version: 0.1-3

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Name: as.corpus.textmeta
    > ### Title: Transform textmeta to corpus
    > ### Aliases: as.corpus.textmeta
    > ### Keywords: manip
    > 
    > ### ** Examples
    > 
    > texts <- list(A="Give a Man a Fish, and You Feed Him for a Day.
    +  Teach a Man To Fish, and You Feed Him for a Lifetime",
    +  B="So Long, and Thanks for All the Fish",
    +  C="A very able manipulative mathematician, Fisher enjoys a real mastery
    +  in evaluating complicated multiple integrals.")
    > 
    > obj <- textmeta(meta=data.frame(id=c("A", "B", "C", "D"),
    +  title=c("Fishing", "Don't panic!", "Sir Ronald", "Berlin"),
    +  date=c("1885-01-02", "1979-03-04", "1951-05-06", "1967-06-02"),
    +  additionalVariable=1:4, stringsAsFactors=FALSE), text=texts)
    > 
    > corp <- as.corpus.textmeta(obj)
    Error: 'metadoc<-' is not an exported object from 'namespace:quanteda'
    Execution halted
    ```

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'as.corpus.textmeta.Rd':
      ‘[quanteda]{metadoc}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘quanteda::metadoc’
    ```

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

