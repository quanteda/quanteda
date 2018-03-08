
[![quanteda: quantitative analysis of textual
data](https://cdn.rawgit.com/quanteda/quanteda/master/images/quanteda_logo.svg)](http://quanteda.io)

[![CRAN
Version](https://www.r-pkg.org/badges/version/quanteda)](https://CRAN.R-project.org/package=quanteda)
[![Downloads](https://cranlogs.r-pkg.org/badges/quanteda)](https://CRAN.R-project.org/package=quanteda)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/quanteda?color=orange)](https://CRAN.R-project.org/package=quanteda)
[![Travis-CI Build
Status](https://travis-ci.org/quanteda/quanteda.svg?branch=master)](https://travis-ci.org/quanteda/quanteda)
[![Appveyor Build
status](https://ci.appveyor.com/api/projects/status/e3tf2h1ff0nlv249/branch/master?svg=true)](https://ci.appveyor.com/project/kbenoit/quanteda/branch/master)
[![codecov](https://codecov.io/gh/quanteda/quanteda/branch/master/graph/badge.svg)](https://codecov.io/gh/quanteda/quanteda)
[![DOI](https://zenodo.org/badge/5424649.svg)](https://zenodo.org/badge/latestdoi/5424649)

## About

An R package for managing and analyzing text, created by [Kenneth
Benoit](http://kenbenoit.net) in collaboration with a team of core
[contributors](https://github.com/quanteda/quanteda/graphs/contributors):
[Kohei Watanabe](https://github.com/koheiw), [Paul
Nulty](https://github.com/pnulty), [Adam
Obeng](https://github.com/adamobeng), [Stefan
Müller](http://muellerstefan.net), [Haiyan
Wang](https://github.com/HaiyanLW), [Ben
Lauderdale](https://github.com/lauderdale), and [Will
Lowe](https://github.com/conjugateprior).  
Supported by the European Research Council grant ERC-2011-StG
283794-QUANTESS.

For more details, see <http://docs.quanteda.io> and the **quanteda**
[vignettes](https://quanteda.io/help/).

## How to Install

The normal way from CRAN, using your R GUI or

``` r
install.packages("quanteda") 
```

Or for the latest development version:

``` r
# devtools package required to install quanteda from Github 
devtools::install_github("quanteda/quanteda") 
```

Because this compiles some C++ source code, you will need a compiler
installed. **If you are using a Windows platform**, this means you will
need also to install the
[Rtools](https://CRAN.R-project.org/bin/windows/Rtools/) software
available from CRAN. **If you are using macOS**, you will need to to
install XCode, available for free from the App Store, or if you prefer a
lighter footprint set of tools, [just the Xcode command line
tools](http://osxdaily.com/2014/02/12/install-command-line-tools-mac-os-x/),
using the command `xcode-select --install` from the Terminal.

## How to Use

See the [quick start
quide](http://docs.quanteda.io/articles/pkgdown/quickstart.html) to
learn how to use **quanteda**.

## Leaving Feedback

If you like **quanteda**, please consider leaving [feedback or a
testimonial here](https://github.com/quanteda/quanteda/issues/461).

## Contributing

Contributions in the form of feedback, comments, code, and bug reports
are most welcome. How to contribute:

  - Fork the source code, modify, and issue a [pull
    request](https://help.github.com/articles/creating-a-pull-request-from-a-fork/)
    through the [project GitHub
    page](https://github.com/quanteda/quanteda). See our [Contributor
    Code of
    Conduct](https://github.com/quanteda/quanteda/blob/master/CONDUCT.md)
    and the all-important **quanteda** [Style
    Guide](https://github.com/quanteda/quanteda/wiki/Style-guide).
  - Issues, bug reports, and wish lists: [File a GitHub
    issue](https://github.com/quanteda/quanteda/issues).
  - Usage questions: Submit a question on the [**quanteda** channel on
    StackOverflow](http://stackoverflow.com/questions/tagged/quanteda).
  - Contact [the maintainer](kbenoit@lse.ac.uk) by email.
