<!-- README.md is generated from README.Rmd. Please edit that file -->
quanteda: Quantitative Analysis of Textual Data
===============================================

[![CRAN Version](http://www.r-pkg.org/badges/version/quanteda)](http://cran.r-project.org/package=quanteda) ![Downloads](http://cranlogs.r-pkg.org/badges/quanteda) [![Travis-CI Build Status](https://travis-ci.org/kbenoit/quanteda.svg?branch=master)](https://travis-ci.org/kbenoit/quanteda) [![codecov.io](https://codecov.io/github/kbenoit/quanteda/coverage.svg?branch=master)](https://codecov.io/gh/kbenoit/quanteda/branch/master)

See the [Getting Started Vignette](http://htmlpreview.github.com/?https://github.com/kbenoit/quanteda/blob/master/vignettes/quickstart.html).

An R package for managing and analyzing text, by Ken Benoit and Paul Nulty.

**quanteda** makes it easy to manage texts in the form of a corpus, defined as a collection of texts that includes document-level variables specific to each text, as well as meta-data for documents and for the collection as a whole. **quanteda** includes tools to make it easy and fast to manuipulate the texts in a corpus, by performing the most common natural language processing tasks simply and quickly, such as tokenizing, stemming, or forming ngrams. **quanteda**'s functions for tokenizing texts and forming multiple tokenized documents into a *document-feature matrix* are both extremely fast and extremely simple to use. **quanteda** can segment texts easily by words, paragraphs, sentences, or even user-supplied delimiters and tags.

Built on the text processing functions in the **stringi** package, which is in turn built on C++ implementation of the [ICU](http://www.icu-project.org/) libraries for Unicode text handling, **quanteda** pays special attention to fast and correct implementation of Unicode and the handling of text in any character set, following conversion internally to UTF-8.

**quanteda** is built for efficiency and speed, through its design around three infrastructures: the **stringi** package for text processing, the **data.table** package for indexing large documents efficiently, and the **Matrix** package for sparse matrix objects. If you can fit it into memory, **quanteda** will handle it quickly. (And eventually, we will make it possible to process objects even larger than available memory.)

**quanteda** is principally designed to allow users a fast and convenient method to go from a corpus of texts to a selected matrix of documents by features, after defining and selecting the documents and features. The package makes it easy to redefine documents, for instance by splitting them into sentences or paragraphs, or by tags, as well as to group them into larger documents by document variables, or to subset them based on logical conditions or combinations of document variables. The package also implements common NLP feature selection functions, such as removing stopwords and stemming in numerous languages, selecting words found in dictionaries, treating words as equivalent based on a user-defined "thesaurus", and trimming and weighting features based on document frequency, feature frequency, and related measures such as *tf-idf*.

Once constructed, a **quanteda** "dfm"" can be easily analyzed using either quanteda's built-in tools for scaling document positions, or used with a number of other text analytic tools, such as:

-   topic models (including converters for direct use with the **topicmodels**, **LDA**, and **stm** packages)

-   document scaling (using **quanteda**'s own functions for the "wordfish" and "Wordscores" models, direct use with the **ca** package for correspondence analysis, or scaling with the **austin** package)

-   machine learning through a variety of other packages that take matrix or matrix-like inputs.

**Additional features** of quanteda include:

-   the ability to explore texts using *key-words-in-context*;

-   fast computation of a variety of readability indexes;

-   fast computation of a variety of lexical diversity measures;

-   quick computation of word or document association measures, for clustering or to compute similarity scores for other purposes; and

-   a comprehensive suite of descriptive statistics on text such as the number of sentences, words, characters, or syllables per document.

**Planned features** coming soon to **quanteda** are:

-   bootstrapping methods for texts that makes it easy to resample texts from pre-defined units, to facilitate computation of confidence intervals on textual statistics using techniques of non-parametric bootstrapping, but applied to the original texts as data.

-   expansion of predictive and analytic methods called through the standard interface called `textmodel()`. Current model types include correspondence analysis, "Wordscores", "Wordfish", and Naive Bayes.

-   Addition of settings to corpus projects, that will propogate through downstream objects.

-   Addition of a history that will propogate through downstream objects.

**Acknowledgements**: This research was supported by the European Research Council grant ERC-2011-StG 283794-QUANTESS.

How to Install
--------------

As of version 0.8.0, the GitHub master repository will always contain the development version of quanteda, while the CRAN version will contain the latest "stable" version. You therefore have two options for installing the package:

1.  From CRAN, using your R package installer, or simply

    ``` r
    install.packages("quanteda")
    ```

2.  (For the development version) From GitHub, using

    ``` r
    # devtools packaged required to install quanteda from Github
    devtools::install_github("kbenoit/quanteda")
    ```

    Because this compiles some C++ source code, you will need a compiler installed. If you are using a Windows platform, this means you will need also to install the [Rtools](http://cran.r-project.org/bin/windows/Rtools/) software available from CRAN. If you are using OS X, you will need to to install XCode, available for free from the App Store, or if you prefer a lighter footprint set of tools, [just the Xcode command line tools](http://osxdaily.com/2014/02/12/install-command-line-tools-mac-os-x/), using the command `xcode-select --install` from the Terminal.

3.  (Optional) You can install some additional corpus data from **quantedaData** using

    ``` r
    devtools::install_github("kbenoit/quantedaData")
    ```

Example usage
-------------

``` r
library(quanteda)
#> quanteda version 0.9.7.7
#> 
#> Attaching package: 'quanteda'
#> The following object is masked from 'package:base':
#> 
#>     sample
# create a corpus from the immigration texts from UK party platforms
uk2010immigCorpus <- corpus(ukimmigTexts,
                            docvars=data.frame(party=names(ukimmigTexts)),
                            notes="Immigration-related sections of 2010 UK party manifestos",
                            enc="UTF-8")
#> Warning in corpus.character(ukimmigTexts, docvars = data.frame(party =
#> names(ukimmigTexts)), : Argument enc not used.
uk2010immigCorpus
#> Corpus consisting of 9 documents and 1 docvar.
summary(uk2010immigCorpus, showmeta=TRUE)
#> Corpus consisting of 9 documents.
#> 
#>          Text Types Tokens Sentences        party
#>           BNP  1126   3330        88          BNP
#>     Coalition   144    268         4    Coalition
#>  Conservative   252    503        15 Conservative
#>        Greens   325    687        21       Greens
#>        Labour   296    703        29       Labour
#>        LibDem   257    499        14       LibDem
#>            PC    80    118         5           PC
#>           SNP    90    136         4          SNP
#>          UKIP   346    739        27         UKIP
#> 
#> Source:  /Users/kbenoit/Dropbox/GitHub/quanteda/* on x86_64 by kbenoit
#> Created: Tue Jun 28 11:25:31 2016
#> Notes:   Immigration-related sections of 2010 UK party manifestos

# key words in context for "deport", 3 words of context
kwic(uk2010immigCorpus, "deport", 3)
#>                        contextPre keyword                contextPost
#>  [BNP, 159]        The BNP will [  deport ] all foreigners convicted
#> [BNP, 1970]                . 2. [  Deport ] all illegal immigrants  
#> [BNP, 1976] immigrants We shall [  deport ] all illegal immigrants  
#> [BNP, 2621]  Criminals We shall [  deport ] all criminal entrants

# create a dfm, removing stopwords
mydfm <- dfm(uk2010immigCorpus, ignoredFeatures=c("will", stopwords("english")))
#> Creating a dfm from a corpus ...
#>    ... lowercasing
#>    ... tokenizing
#>    ... indexing documents: 9 documents
#>    ... indexing features: 1,585 feature types
#>    ... removed 97 features, from 175 supplied (glob) feature types
#>    ... created a 9 x 1489 sparse dfm
#>    ... complete. 
#> Elapsed time: 0.023 seconds.
dim(mydfm)              # basic dimensions of the dfm
#> [1]    9 1489
topfeatures(mydfm, 20)  # 20 top words
#> immigration     british      people      asylum     britain          uk 
#>          66          37          35          29          28          27 
#>      system  population     country         new  immigrants      ensure 
#>          27          21          20          19          17          17 
#>       shall citizenship      social    national         bnp     illegal 
#>          17          16          14          14          13          13 
#>        work     percent 
#>          13          12
plot(mydfm, min.freq = 6, random.order = FALSE)             # word cloud     
```

![](images/quanteda_example-1.png)

Documentation
-------------

In-depth tutorials in the form of a gitbook will be available here [here](http://kbenoit.github.io/quanteda).

Examples for any function can also be seen using (for instance, for `corpus()`):

``` r
example(corpus)
```

There are also some demo functions that show off some of the package capabilities, such as `demo(quanteda)`.
