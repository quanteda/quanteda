<!-- README.md is generated from README.Rmd. Please edit that file -->
[![quanteda](images/quanteda_logo.png)](https://kbenoit.github.io/quanteda/) [![CRAN Version](https://www.r-pkg.org/badges/version/quanteda)](https://CRAN.R-project.org/package=quanteda) [![Downloads](https://cranlogs.r-pkg.org/badges/quanteda)](https://CRAN.R-project.org/package=quanteda) [![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/quanteda?color=orange)](https://CRAN.R-project.org/package=quanteda) [![Travis-CI Build Status](https://travis-ci.org/kbenoit/quanteda.svg?branch=master)](https://travis-ci.org/kbenoit/quanteda) [![Build status](https://ci.appveyor.com/api/projects/status/e3tf2h1ff0nlv249/branch/master?svg=true)](https://ci.appveyor.com/project/kbenoit/quanteda/branch/master) [![codecov.io](https://codecov.io/github/kbenoit/quanteda/coverage.svg?branch=master)](https://codecov.io/gh/kbenoit/quanteda/branch/master)

**quanteda** v0.9.9 under development
-------------------------------------

This version of the package is a transitional release prior to v1.0. It includes some major API changes (see below), but with the most of the older functions retained and deprecated. v0.9.9 also implements many enhancements and performance improvements. See [Quanteda Structure and Design](https://kbenoit.github.io/quanteda/articles/development-plans.html) for details.

About the package
-----------------

An R package for managing and analyzing text, created by [Kenneth Benoit](kbenoit@lse.ac.uk) in collaboration with a team of core [contributors](https://github.com/kbenoit/quanteda/graphs/contributors): [Paul Nulty](https://github.com/pnulty), [Adam Obeng](https://github.com/adamobeng), [Kohei Watanabe](https://github.com/koheiw), [Haiyan Wang](https://github.com/HaiyanLW), [Ben Lauderdale](https://github.com/lauderdale), and [Will Lowe](https://github.com/conjugateprior).
Supported by the European Research Council grant ERC-2011-StG 283794-QUANTESS.

For more details, see the [**package website**](https://kbenoit.github.io/quanteda/).

### Leave feedback

If you like **quanteda**, please consider leaving [feedback or a testimonial here](https://github.com/kbenoit/quanteda/issues/461).

Features
--------

### Powerful text analytics

**Generalized, flexible corpus management.**
**quanteda** provides a comprehensive workflow and ecosystem for the management, processing, and analysis of texts. Documents and associated document- and collection-level metadata are easily loaded and stored as a *corpus* object, although most of **quanteda**'s operations work on simple character objects as well. A corpus is designed to efficiently store all of the texts in a collection, as well as meta-data for documents and for the collection as a whole. This makes it easy to perform natural language processing on the texts in a corpus simply and quickly, such as tokenizing, stemming, or forming ngrams. **quanteda**'s functions for tokenizing texts and forming multiple tokenized documents into a *document-feature matrix* are both extremely fast and extremely simple to use. **quanteda** can segment texts easily by words, paragraphs, sentences, or even user-supplied delimiters and tags.

**Works nicely with UTF-8**.
Built on the text processing functions in the **stringi** package, which is in turn built on C++ implementation of the [ICU](http://www.icu-project.org/) libraries for Unicode text handling, **quanteda** pays special attention to fast and correct implementation of Unicode and the handling of text in any character set, following conversion internally to UTF-8.

**Built for efficiency and speed**.
All of the functions in **quanteda** are built for maximum performance and scale while still being as R-based as possible. The package makes use of three efficient architectural elements:
the **stringi** package for text processing, the **Matrix** package for sparse matrix objects, and the **data.table** package for indexing large documents efficiently. If you can fit it into memory, **quanteda** will handle it quickly. (And eventually, we will make it possible to process objects even larger than available memory.)

**Super-fast conversion of texts into a document-feature matrix**.
**quanteda** is principally designed to allow users a fast and convenient method to go from a corpus of texts to a selected matrix of documents by features, after defining and selecting the documents and features. The package makes it easy to redefine documents, for instance by splitting them into sentences or paragraphs, or by tags, as well as to group them into larger documents by document variables, or to subset them based on logical conditions or combinations of document variables. A special variation of the "dfm", a *feature co-occurrence matrix*, is also implemented, for direct use with embedding and representational models such as [**text2vec**](https://github.com/dselivanov/text2vec).

**Extensive feature selection capabilities**.
The package also implements common NLP feature selection functions, such as removing stopwords and stemming in numerous languages, selecting words found in dictionaries, treating words as equivalent based on a user-defined "thesaurus", and trimming and weighting features based on document frequency, feature frequency, and related measures such as *tf-idf*.

**Qualitative exploratory tools**.
Easily search and save *keywords in context*, for instance, or identify keywords. Like all of **quanteda**'s pattern matching functions, users have the option of simple "[glob](https://en.wikipedia.org/wiki/Glob_(programming))" expressions, regular expressions, or fixed pattern matches.

**Dictionary-based analysis**.
**quanteda** allows fast and flexible implementation of dictionary methods, including the import and conversion of foreign dictionary formats such as those from Provalis's WordStat, the Linguistic Inquiry and Word Count (LIWC), Lexicoder, and Yoshioder.

**Text analytic methods**.
Once constructed, a *dfm* can be easily analyzed using either **quanteda**'s built-in tools for scaling document positions (for the "wordfish" and "Wordscores" models, direct use with the [**ca**](https://CRAN.R-project.org/package=ca) package for correspondence analysis), predictive models using Naive Bayes multinomial and Bernoulli classifiers, computing distance or similarity matrixes of features or documents, or computing readability or lexical diversity indexes.

In addition, **quanteda** a document-feature matrix is easily used with or converted for a number of other text analytic tools, such as:

-   *topic models* (including converters for direct use with the [**topicmodels**](https://CRAN.R-project.org/package=topicmodels), [**LDA**](https://CRAN.R-project.org/package=lda), and [**stm**](http://www.structuraltopicmodel.com) packages);

-   machine learning through a variety of other packages that take matrix or matrix-like inputs.

**Planned features.** Coming soon to **quanteda** are:

-   *Bootstrapping methods* for texts that makes it easy to resample texts from pre-defined units, to facilitate computation of confidence intervals on textual statistics using techniques of non-parametric bootstrapping, but applied to the original texts as data.

-   *Additional predictive and analytic methods* by expanding the `textstat_` and `textmodel_` functions. Current textmodel types include correspondence analysis, "Wordscores", "Wordfish", and Naive Bayes; current textstat statistics are readability, lexical diversity, similarity, and distance.

-   *Expanded settings* for all objects, that will propogate through downstream objects.

-   *Object histories*, that will propogate through downstream objects, to enhance analytic reproducibility and transparency.

How to Install
--------------

1.  From [CRAN](https://CRAN.R-project.org/package=quanteda): Use your GUI's R package installer, or execute:

    ``` r
    install.packages("quanteda") 
    ```

2.  From [GitHub](https://github.com/kbenoit/quanteda), using:

    ``` r
    # devtools packaged required to install quanteda from Github 
    devtools::install_github("kbenoit/quanteda") 
    ```

    Because this compiles some C++ source code, you will need a compiler installed. If you are using a Windows platform, this means you will need also to install the [Rtools](https://CRAN.R-project.org/bin/windows/Rtools/) software available from CRAN. If you are using OS X, you will need to to install XCode, available for free from the App Store, or if you prefer a lighter footprint set of tools, [just the Xcode command line tools](http://osxdaily.com/2014/02/12/install-command-line-tools-mac-os-x/), using the command `xcode-select --install` from the Terminal.

3.  Additional recommended packages:

    The following packages work well with **quanteda** and we recommend that you also install them:

    -   [**readtext**](https://github.com/kbenoit/readtext): For reading text data into R.

        ``` r
        devtools::install_github("kbenoit/readtext")
        ```

    -   [**quantedaData**](https://github.com/kbenoit/quantedaData): Additional textual data for use with **quanteda**.

        ``` r
        r devtools::install_github("kbenoit/quantedaData")
        ```

    -   [**spacyr**](https://github.com/kbenoit/spacyr): NLP using the [spaCy](http://spacy.io) library.

Getting Started
---------------

See the [package website](http://kbenoit.github.io/quanteda), which includes the [Getting Started Vignette](https://kbenoit.github.io/quanteda/articles/quickstart.html).

Demonstration
-------------

``` r
library(quanteda)
## quanteda version 0.9.9.15
## 
## Attaching package: 'quanteda'
## The following object is masked from 'package:utils':
## 
##     View
## The following object is masked from 'package:base':
## 
##     sample

# create a corpus from the immigration texts from UK party platforms
uk2010immigCorpus <- 
    corpus(data_char_ukimmig2010,
           docvars = data.frame(party = names(data_char_ukimmig2010)),
           metacorpus = list(notes = "Immigration-related sections of 2010 UK party manifestos"))
uk2010immigCorpus
## Corpus consisting of 9 documents and 1 docvar.
summary(uk2010immigCorpus)
## Corpus consisting of 9 documents.
## 
##          Text Types Tokens Sentences        party
##           BNP  1126   3330        88          BNP
##     Coalition   144    268         4    Coalition
##  Conservative   252    503        15 Conservative
##        Greens   325    687        21       Greens
##        Labour   296    703        29       Labour
##        LibDem   257    499        14       LibDem
##            PC    80    118         5           PC
##           SNP    90    136         4          SNP
##          UKIP   346    739        27         UKIP
## 
## Source:  /home/kohei/packages/quanteda/* on x86_64 by kohei
## Created: Wed Jan 25 07:36:11 2017
## Notes:   Immigration-related sections of 2010 UK party manifestos

# key words in context for "deport", 3 words of context
kwic(uk2010immigCorpus, "deport", 3)
##                                                                    
## [BNP, 159]         The BNP will | deport | all foreigners convicted
## [BNP, 1970]                . 2. | Deport | all illegal immigrants  
## [BNP, 1976] immigrants We shall | deport | all illegal immigrants  
## [BNP, 2621]  Criminals We shall | deport | all criminal entrants

# create a dfm, removing stopwords
mydfm <- dfm(uk2010immigCorpus, remove = c("will", stopwords("english")),
             removePunct = TRUE)
mydfm
## Document-feature matrix of: 9 documents, 1,547 features (83.8% sparse).

topfeatures(mydfm, 20)  # 20 top words
## immigration     british      people      asylum     britain          uk 
##          66          37          35          29          28          27 
##      system  population     country         new  immigrants      ensure 
##          27          21          20          19          17          17 
##       shall citizenship      social    national         bnp     illegal 
##          17          16          14          14          13          13 
##        work     percent 
##          13          12

# plot a word cloud
textplot_wordcloud(mydfm, min.freq = 6, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))
```

![](images/quanteda_example-1.png)

Contributing
------------

Contributions in the form of feedback, comments, code, and bug reports are most welcome. How to contribute:

-   Fork the source code, modify, and issue a [pull request](https://help.github.com/articles/creating-a-pull-request-from-a-fork/) through the [project GitHub page](https://github.com/kbenoit/quanteda). See our [Contributor Code of Conduct](https://github.com/kbenoit/quanteda/blob/master/CONDUCT.md).

-   Issues, bug reports, and wish lists: [File a GitHub issue](https://github.com/kbenoit/quanteda/issues).

-   Usage questions: Submit a question on the [**quanteda** channel on StackOverflow](http://stackoverflow.com/questions/tagged/quanteda).

-   Contact [the maintainer](kbenoit@lse.ac.uk) by email.
