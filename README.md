
[![quanteda: quantitative analysis of textual
data](https://cdn.rawgit.com/quanteda/quanteda/master/images/quanteda_logo.svg)](http://quanteda.io)

<!-- badges: start -->

[![CRAN
Version](https://www.r-pkg.org/badges/version/quanteda)](https://CRAN.R-project.org/package=quanteda)
[![](https://img.shields.io/badge/devel%20version-4.2.0-royalblue.svg)](https://github.com/quanteda/quanteda)
[![Downloads](https://cranlogs.r-pkg.org/badges/quanteda)](https://CRAN.R-project.org/package=quanteda)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/quanteda?color=orange)](https://CRAN.R-project.org/package=quanteda)
[![R-CMD-check](https://github.com/quanteda/quanteda/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/quanteda/quanteda/actions/workflows/check-standard.yaml)
[![codecov](https://codecov.io/gh/quanteda/quanteda/branch/master/graph/badge.svg)](https://app.codecov.io/gh/quanteda/quanteda)
[![DOI](https://zenodo.org/badge/5424649.svg)](https://zenodo.org/badge/latestdoi/5424649)
[![DOI](http://joss.theoj.org/papers/10.21105/joss.00774/status.svg)](https://doi.org/10.21105/joss.00774)
<!-- badges: end -->

## About

**quanteda** is an R package for managing and analyzing text, created
and maintained by [Kenneth Benoit](https://kenbenoit.net) and [Kohei
Watanabe](https://blog.koheiw.net/). Its creation was funded by the
European Research Council grant ERC-2011-StG 283794-QUANTESS and its
continued development is supported by the [Quanteda Initiative
CIC](https://quanteda.org).

For more details, see <https://quanteda.io>.

## **quanteda** version 4

The **quanteda** 4.0 is a major release that improves functionality and
performance and further improves function consistency by removing
previously deprecated functions. It also includes significant new
tokeniser rules that make the default tokeniser smarter than ever, with
new Unicode and ICU-compliant rules enabling it to work more
consistently with even more languages.

We describe more fully these significant changes in:

- an [article about the new external pointer tokens
  objects](https://quanteda.io/articles/pkgdown/tokens_xptr.html);
- an [article showing performance
  benchmarks](https://quanteda.io/articles/pkgdown/benchmarks_xptr.html)
  for the new external pointer tokens objects, as well as some of the
  tokeniser improvements in v4; and
- the [changelog for
  v4](https://github.com/quanteda/quanteda/blob/master/NEWS.md#quanteda-40)
  a full listing of the changes, improvements, and deprecations in v4.

## The **quanteda** family of packages

We completed the trend of splitting **quanteda** into modular packages
with the release of v3. The quanteda family of packages includes the
following:

- [**quanteda**](https://github.com/quanteda/quanteda): contains all of
  the core natural language processing and textual data management
  functions
- [**quanteda.textmodels**](https://github.com/quanteda/quanteda.textmodels):
  contains all of the text models and supporting functions, namely the
  `textmodel_*()` functions. This was split from the main package with
  the v2 release
- [**quanteda.textstats**](https://github.com/quanteda/quanteda.textstats):
  statistics for textual data, namely the `textstat_*()` functions,
  split with the v3 release
- [**quanteda.textplots**](https://github.com/quanteda/quanteda.textplots):
  plots for textual data, namely the `textplot_*()` functions, split
  with the v3 release

We are working on additional package releases, available in the meantime
from our GitHub pages:

- [**quanteda.sentiment**](https://github.com/quanteda/quanteda.sentiment):
  Functions and lexicons for sentiment analysis using dictionaries
- [**quanteda.tidy**](https://github.com/quanteda/quanteda.tidy):
  Extensions for manipulating document variables in core **quanteda**
  objects using your favourite **tidyverse** functions

and more to come.

## How To…

### Install (binaries) from CRAN

The normal way from CRAN, using your R GUI or

``` r
install.packages("quanteda") 
```

**(New for quanteda v4.0)** For Linux users: Because all installations
on Linux are compiled, Linux users will first need to install the Intel
oneAPI Threading Building Blocks for parallel computing for installation
to work.

To install TBB on Linux:

``` bash
# Fedora, CentOS, RHEL
sudo yum install tbb-devel

# Debian and Ubuntu
sudo apt install libtbb-dev
```

### Compile from source (macOS and Windows)

Because this compiles some C++ and Fortran source code, you will need to
have installed the appropriate compilers to build the development
version.

You will also need to install TBB:

**macOS:**

First, you will need to install XCode command line tools.

``` bash
xcode-select --install
```

Then install the TBB libraries and the pkg-config utility: (after
installing [Homebrew](https://brew.sh)):

``` bash
brew install tbb pkg-config
```

Finally, you will need to install
[gfortran](https://github.com/fxcoudert/gfortran-for-macOS/releases).

**Windows:**

Install [RTools](https://cran.r-project.org/bin/windows/Rtools/), which
includes the TBB libraries.

### Enable parallelisation

**quanteda** takes advantage of parallel computing through the [TBB
(Threading Building Blocks)
library](https://en.wikipedia.org/wiki/Threading_Building_Blocks) to
speed up computations. This guide provides step-by-step instructions on
how to set up your system for using Quanteda with parallel capabilities
on Windows, macOS, and Linux.

**Windows:**

Download and install RTools from [RTools download
page](https://cran.r-project.org/bin/windows/Rtools/).

**macOS:**

1.  **Install XCode Command Line Tools**
    - Type the following command in the terminal:

      ``` bash
      xcode-select --install
      ```
2.  **Install Homebrew**
    - If Homebrew is not installed, run:

      ``` bash
      /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
      ```
3.  **Install TBB and pkg-config**
    - After installing Homebrew, run:

      ``` bash
      brew install tbb pkg-config
      ```
4.  **Install gfortran**
    - Required for compiling Fortran code, install using Homebrew:

      ``` bash
      brew install gcc
      ```

**Linux:**

Install TBB:

- For Fedora, CentOS, RHEL:

  ``` bash
  sudo yum install tbb-devel
  ```

- For Debian and Ubuntu:

  ``` bash
  sudo apt install libtbb-dev
  ```

More details are provided in the [quanteda
documentation](http://quanteda.io/articles/pkgdown/parallelisation.html).

### Use **quanteda**

See the [quick start
guide](https://quanteda.io/articles/quickstart.html) to learn how to use
**quanteda**.

### Get Help

- Read out documentation at <https://quanteda.io>.
- Check out the [**quanteda**
  cheatsheet](https://github.com/quanteda/quanteda/blob/master/tests/cheatsheet/quanteda-cheatsheet.pdf).
- Submit a question on the [**quanteda** channel on
  StackOverflow](https://stackoverflow.com/questions/tagged/quanteda).
- See our [tutorial site](https://tutorials.quanteda.io/).

### Cite the package

Benoit, Kenneth, Kohei Watanabe, Haiyan Wang, Paul Nulty, Adam Obeng,
Stefan Müller, and Akitaka Matsuo. (2018) “[quanteda: An R package for
the quantitative analysis of textual
data](https://www.theoj.org/joss-papers/joss.00774/10.21105.joss.00774.pdf)”.
*Journal of Open Source Software* 3(30), 774.
<https://doi.org/10.21105/joss.00774>.

For a BibTeX entry, use the output from
`citation(package = "quanteda")`.

### Leave Feedback

If you like **quanteda**, please consider leaving [feedback or a
testimonial here](https://github.com/quanteda/quanteda/issues/461).

### Contribute

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
