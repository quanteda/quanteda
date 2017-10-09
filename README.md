
<!-- README.md is generated from README.Rmd. Please edit that file -->
### [![quanteda: quantitative analysis of textual data](https://cdn.rawgit.com/kbenoit/quanteda/master/images/quanteda_logo.svg)](http://quanteda.io)

[![CRAN Version](https://www.r-pkg.org/badges/version/quanteda)](https://CRAN.R-project.org/package=quanteda) [![Downloads](https://cranlogs.r-pkg.org/badges/quanteda)](https://CRAN.R-project.org/package=quanteda) [![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/quanteda?color=orange)](https://CRAN.R-project.org/package=quanteda) [![Travis-CI Build Status](https://travis-ci.org/kbenoit/quanteda.svg?branch=master)](https://travis-ci.org/kbenoit/quanteda) [![Build status](https://ci.appveyor.com/api/projects/status/e3tf2h1ff0nlv249/branch/master?svg=true)](https://ci.appveyor.com/project/kbenoit/quanteda/branch/master) [![codecov.io](https://codecov.io/github/kbenoit/quanteda/coverage.svg?branch=master)](https://codecov.io/gh/kbenoit/quanteda/branch/master)

Important Changes in v0.99
--------------------------

Many important changes to the package have been underway, including API changes, as we approach a stable API, "1.0" release in October 2017. Version 0.99 represents the last version that will contain many of the deprecated object types and methods that date several releases.

v0.99 also implements many enhancements and performance improvements over previous releases. See [NEWS.md](http://quanteda.io/news/index.html#quanteda-0-99) for details, and [Quanteda Structure and Design](https://kbenoit.github.io/quanteda/articles/development-plans.html) for a description of the package's underlying logic and design philosophy.

About the package
-----------------

An R package for managing and analyzing text, created by [Kenneth Benoit](http://kenbenoit.net) in collaboration with a team of core [contributors](https://github.com/kbenoit/quanteda/graphs/contributors): [Kohei Watanabe](https://github.com/koheiw), [Paul Nulty](https://github.com/pnulty), [Adam Obeng](https://github.com/adamobeng), [Haiyan Wang](https://github.com/HaiyanLW), [Ben Lauderdale](https://github.com/lauderdale), and [Will Lowe](https://github.com/conjugateprior).
Supported by the European Research Council grant ERC-2011-StG 283794-QUANTESS.

For more details, see the [**package website**](http://quanteda.io).

How to cite the package:


    To cite package 'quanteda' in publications please use the
    following:

      Benoit, Kenneth et. al. ().  "quanteda: Quantitative Analysis of
      Textual Data".  R package version: 0.99.13.  http://quanteda.io.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {quanteda: Quantitative Analysis of Textual Data},
        author = {Kenneth Benoit and Kohei Watanabe and Paul Nulty and Adam Obeng and Haiyan Wang and Benjamin Lauderdale and Will Lowe},
        note = {R package version 0.99.13},
        url = {http://quanteda.io},
      }

Getting Started
---------------

See the [package website](http://quanteda.io), which includes the [Getting Started Vignette](http://quanteda.io/articles/quickstart.html).

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

    The following packages work well with or extend **quanteda** and we recommend that you also install them:

    -   [**readtext**](https://github.com/kbenoit/readtext): An easy way to read text data into R, from almost any input format.

    -   [**spacyr**](https://github.com/kbenoit/spacyr): NLP using the [spaCy](http://spacy.io) library, including part-of-speech tagging, entity recognition, and dependency parsing.

    -   [**quantedaData**](https://github.com/kbenoit/quantedaData): Additional textual data for use with **quanteda**.

        ``` r
        devtools::install_github("kbenoit/quantedaData")
        ```

    -   [**LIWCalike**](https://github.com/kbenoit/LIWCalike): An R implementation of the [Linguistic Inquiry and Word Count](http://liwc.wpengine.com) approach to text analysis.

        ``` r
        devtools::install_github("kbenoit/LIWCalike")
        ```

Leave feedback
--------------

If you like **quanteda**, please consider leaving [feedback or a testimonial here](https://github.com/kbenoit/quanteda/issues/461).

For developpers
---------------

### Basic principles

1.  Corpus texts are text repositories.
    -   Should not have their texts modified as part of preparation or analysis
    -   Subsetting or redefining documents is allowable

2.  A corpus should be capable of holding additional objects that will be associated with the corpus, such as dictionaries, stopword, and phrase lists, that will propagate downstream.
3.  Downstream objects should record the settings used to produce them.
    -   includes: tokenized texts (`tokens`), document-feature matrixes (`dfm`)
    -   settings examples are: `tolower`, `stem`, `removeTwitter` etc.
    -   also include any objects used in feature selection, such as dictionaries or stopword lists

4.  A document-feature matrix is a sparse matrix that is always *documents* (or document groups) in rows by *features* in columns.
5.  Encoding of texts should be done in the corpus, and recorded as meta-data in the corpus
    -   This encoding should be `UTF-8` by default (problem for Windows machines)
    -   Basic text operations will use the `stringi` package, based on the ICU libraries for Unicode compliance

### Contributing

Contributions in the form of feedback, comments, code, and bug reports are most welcome. How to contribute:

-   Fork the source code, modify, and issue a [pull request](https://help.github.com/articles/creating-a-pull-request-from-a-fork/) through the [project GitHub page](https://github.com/kbenoit/quanteda). See our [Contributor Code of Conduct](https://github.com/kbenoit/quanteda/blob/master/CONDUCT.md) and the all-important **quanteda** [Style Guide](https://github.com/kbenoit/quanteda/wiki/Style-guide).
-   Issues, bug reports, and wish lists: [File a GitHub issue](https://github.com/kbenoit/quanteda/issues).
-   Usage questions: Submit a question on the [**quanteda** channel on StackOverflow](http://stackoverflow.com/questions/tagged/quanteda).
-   Contact [the maintainer](kbenoit@lse.ac.uk) by email.
