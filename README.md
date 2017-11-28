
[![quanteda: quantitative analysis of textual data](https://cdn.rawgit.com/kbenoit/quanteda/master/images/quanteda_logo.svg)](http://quanteda.io)

[![CRAN Version](https://www.r-pkg.org/badges/version/quanteda)](https://CRAN.R-project.org/package=quanteda) [![Downloads](https://cranlogs.r-pkg.org/badges/quanteda)](https://CRAN.R-project.org/package=quanteda) [![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/quanteda?color=orange)](https://CRAN.R-project.org/package=quanteda) [![Travis-CI Build Status](https://travis-ci.org/kbenoit/quanteda.svg?branch=master)](https://travis-ci.org/kbenoit/quanteda) [![Appveyor Build status](https://ci.appveyor.com/api/projects/status/e3tf2h1ff0nlv249/branch/master?svg=true)](https://ci.appveyor.com/project/kbenoit/quanteda/branch/master) [![codecov](https://codecov.io/gh/kbenoit/quanteda/branch/master/graph/badge.svg)](https://codecov.io/gh/kbenoit/quanteda) [![DOI](https://zenodo.org/badge/5424649.svg)](https://zenodo.org/badge/latestdoi/5424649)

About
-----

An R package for managing and analyzing text, created by [Kenneth Benoit](http://kenbenoit.net) in collaboration with a team of core [contributors](https://github.com/kbenoit/quanteda/graphs/contributors): [Kohei Watanabe](https://github.com/koheiw), [Paul Nulty](https://github.com/pnulty), [Adam Obeng](https://github.com/adamobeng), [Haiyan Wang](https://github.com/HaiyanLW), [Ben Lauderdale](https://github.com/lauderdale), and [Will Lowe](https://github.com/conjugateprior).
Supported by the European Research Council grant ERC-2011-StG 283794-QUANTESS.

For more details, see <http://quanteda.io>.

How to cite the package:


    Benoit K (2017). _quanteda: Quantitative Analysis of Textual
    Data_. doi: 10.5281/zenodo.1004683 (URL:
    http://doi.org/10.5281/zenodo.1004683), R package version 0.99.22,
    <URL: http://quanteda.io>.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {quanteda: Quantitative Analysis of Textual Data},
        author = {Kenneth Benoit},
        year = {2017},
        doi = {10.5281/zenodo.1004683},
        url = {http://quanteda.io},
        note = {R package version 0.99.22},
      }

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

    Because this compiles some C++ source code, you will need a compiler installed. If you are using a Windows platform, this means you will need also to install the [Rtools](https://CRAN.R-project.org/bin/windows/Rtools/) software available from CRAN. **If you are using macOS**, you will need to to install XCode, available for free from the App Store, or if you prefer a lighter footprint set of tools, [just the Xcode command line tools](http://osxdaily.com/2014/02/12/install-command-line-tools-mac-os-x/), using the command `xcode-select --install` from the Terminal.

    Also, you might need to upgrade your compiler. @kbenoit found that his macOS build only worked reliably after upgrading the default Xcode compiler to clang4, following [these instructions](http://thecoatlessprofessor.com/programming/openmp-in-r-on-os-x/#after-3-4-0).

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

Leaving feedback
----------------

If you like **quanteda**, please consider leaving [feedback or a testimonial here](https://github.com/kbenoit/quanteda/issues/461).

Contributing
------------

Contributions in the form of feedback, comments, code, and bug reports are most welcome. How to contribute:

-   Fork the source code, modify, and issue a [pull request](https://help.github.com/articles/creating-a-pull-request-from-a-fork/) through the [project GitHub page](https://github.com/kbenoit/quanteda). See our [Contributor Code of Conduct](https://github.com/kbenoit/quanteda/blob/master/CONDUCT.md) and the all-important **quanteda** [Style Guide](https://github.com/kbenoit/quanteda/wiki/Style-guide).
-   Issues, bug reports, and wish lists: [File a GitHub issue](https://github.com/kbenoit/quanteda/issues).
-   Usage questions: Submit a question on the [**quanteda** channel on StackOverflow](http://stackoverflow.com/questions/tagged/quanteda).
-   Contact [the maintainer](kbenoit@lse.ac.uk) by email.
