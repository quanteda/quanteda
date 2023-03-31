
[![quanteda: quantitative analysis of textual
data](https://cdn.rawgit.com/quanteda/quanteda/master/images/quanteda_logo.svg)](http://quanteda.io)

<!-- badges: start -->

[![CRAN
Version](https://www.r-pkg.org/badges/version/quanteda)](https://CRAN.R-project.org/package=quanteda)
[![](https://img.shields.io/badge/devel%20version-3.3.0-royalblue.svg)](https://github.com/quanteda/quanteda)
[![Downloads](https://cranlogs.r-pkg.org/badges/quanteda)](https://CRAN.R-project.org/package=quanteda)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/quanteda?color=orange)](https://CRAN.R-project.org/package=quanteda)
[![R-CMD-check](https://github.com/quanteda/quanteda/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/quanteda/quanteda/actions/workflows/check-standard.yaml)
[![codecov](https://codecov.io/gh/quanteda/quanteda/branch/master/graph/badge.svg)](https://app.codecov.io/gh/quanteda/quanteda)
[![DOI](https://zenodo.org/badge/5424649.svg)](https://zenodo.org/badge/latestdoi/5424649)
[![DOI](http://joss.theoj.org/papers/10.21105/joss.00774/status.svg)](https://doi.org/10.21105/joss.00774)
<!-- badges: end -->

## About

An R package for managing and analyzing text, created by [Kenneth
Benoit](https://kenbenoit.net). Supported by the European Research
Council grant ERC-2011-StG 283794-QUANTESS.

For more details, see <https://quanteda.io>.

## **quanteda** version 3

The **quanteda** 3.0 major release improves functionality, completes the
modularisation of the package begun in v2.0, further improves function
consistency by removing previously deprecated functions, and enhances
workflow stability and consistency by deprecating some shortcut steps
built into some functions.

See
<https://github.com/quanteda/quanteda/blob/master/NEWS.md#quanteda-30>
for a full list of the changes.

## The **quanteda** family of packages

As of v3.0, we have continued our trend of splitting **quanteda** into
modular packages. These are now the following:

- **quanteda**: contains all of the core natural language processing and
  textual data management functions
- **quanteda.textmodels**: contains all of the text models and
  supporting functions, namely the `textmodel_*()` functions. This was
  split from the main package with the v2 release
- **quanteda.textstats**: statistics for textual data, namely the
  `textstat_*()` functions, split with the v3 release
- **quanteda.textplots**: plots for textual data, namely the
  `textplot_*()` functions, split with the v3 release

We are working on additional package releases, available in the meantime
from our GitHub pages:

- **quanteda.sentiment**: Functions and lexicons for sentiment analysis
  using dictionaries
- **quanteda.tidy**: Extensions for manipulating document variables in
  core **quanteda** objects using your favourite **tidyverse** functions

and more to come.

## How To…

### How to Install

The normal way from CRAN, using your R GUI or

``` r
install.packages("quanteda") 
```

Or for the latest development version:

``` r
# devtools package required to install quanteda from Github 
remotes::install_github("quanteda/quanteda") 
```

Because this compiles some C++ and Fortran source code, you will need to
have installed the appropriate compilers to build the development
version.

### How to Use

See the [quick start
guide](https://quanteda.io/articles/pkgdown/quickstart.html) to learn
how to use **quanteda**.

### How to Get Help

- Read out documentation at <https://quanteda.io>.
- Submit a question on the [**quanteda** channel on
  StackOverflow](https://stackoverflow.com/questions/tagged/quanteda).
- See our [tutorial site](https://tutorials.quanteda.io/).

### How to Cite

Benoit, Kenneth, Kohei Watanabe, Haiyan Wang, Paul Nulty, Adam Obeng,
Stefan Müller, and Akitaka Matsuo. (2018) “[quanteda: An R package for
the quantitative analysis of textual
data](https://www.theoj.org/joss-papers/joss.00774/10.21105.joss.00774.pdf)”.
*Journal of Open Source Software*. 3(30), 774.
<https://doi.org/10.21105/joss.00774>.

For a BibTeX entry, use the output from
`citation(package = "quanteda")`.

### How to Leave Feedback

If you like **quanteda**, please consider leaving [feedback or a
testimonial here](https://github.com/quanteda/quanteda/issues/461).

### How to Contribute

Contributions in the form of feedback, comments, code, and bug reports
are most welcome. How to contribute:

- Fork the source code, modify, and issue a [pull
  request](https://help.github.com/articles/creating-a-pull-request-from-a-fork/)
  through the [project GitHub
  page](https://github.com/quanteda/quanteda). See our [Contributor Code
  of
  Conduct](https://github.com/quanteda/quanteda/blob/master/CONDUCT.md)
  and the all-important **quanteda** [Style
  Guide](https://github.com/quanteda/quanteda/wiki/Style-guide).
- Issues, bug reports, and wish lists: [File a GitHub
  issue](https://github.com/quanteda/quanteda/issues).
- Contact [the maintainer](mailto:kbenoit@lse.ac.uk) by email.
