
[![quanteda: quantitative analysis of textual
data](https://cdn.rawgit.com/quanteda/quanteda/master/images/quanteda_logo.svg)](http://quanteda.io)

<!-- badges: start -->

[![CRAN
Version](https://www.r-pkg.org/badges/version/quanteda)](https://CRAN.R-project.org/package=quanteda)
[![](https://img.shields.io/badge/devel%20version-2.1.2-royalblue.svg)](https://github.com/quanteda/quanteda)
[![Downloads](https://cranlogs.r-pkg.org/badges/quanteda)](https://CRAN.R-project.org/package=quanteda)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/quanteda?color=orange)](https://CRAN.R-project.org/package=quanteda)
[![R build
status](https://github.com/quanteda/quanteda/workflows/R-CMD-check/badge.svg)](https://github.com/quanteda/quanteda/actions)
[![codecov](https://codecov.io/gh/quanteda/quanteda/branch/master/graph/badge.svg)](https://codecov.io/gh/quanteda/quanteda)
[![DOI](https://zenodo.org/badge/5424649.svg)](https://zenodo.org/badge/latestdoi/5424649)
[![DOI](http://joss.theoj.org/papers/10.21105/joss.00774/status.svg)](https://doi.org/10.21105/joss.00774)
<!-- badges: end -->

## About

An R package for managing and analyzing text, created by [Kenneth
Benoit](https://kenbenoit.net). Supported by the European Research
Council grant ERC-2011-StG 283794-QUANTESS.

For more details, see <https://quanteda.io>.

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

Because this compiles some C++ and Fortran source code, you will need to
have installed the appropriate compilers.

**If you are using a Windows platform**, this means you will need also
to install the [Rtools](https://CRAN.R-project.org/bin/windows/Rtools/)
software available from CRAN.

**If you are using macOS**, you should install the [macOS
tools](https://cran.r-project.org/bin/macosx/tools/), namely the Clang
6.x compiler and the GNU Fortran compiler (as **quanteda** requires
gfortran to build). If you are still getting errors related to gfortran,
follow the fixes
[here](https://thecoatlessprofessor.com/programming/rcpp-rcpparmadillo-and-os-x-mavericks--lgfortran-and--lquadmath-error/).

## How to Use

See the [quick start
guide](https://quanteda.io/articles/pkgdown/quickstart.html) to learn
how to use **quanteda**.

## How to cite

Benoit, Kenneth, Kohei Watanabe, Haiyan Wang, Paul Nulty, Adam Obeng,
Stefan Müller, and Akitaka Matsuo. (2018) “[quanteda: An R package for
the quantitative analysis of textual
data](https://www.theoj.org/joss-papers/joss.00774/10.21105.joss.00774.pdf)”.
*Journal of Open Source Software*. 3(30), 774.
<https://doi.org/10.21105/joss.00774>.

For a BibTeX entry, use the output from `citation(package =
"quanteda")`.

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
    StackOverflow](https://stackoverflow.com/questions/tagged/quanteda).
  - Contact [the maintainer](mailto:kbenoit@lse.ac.uk) by email.
