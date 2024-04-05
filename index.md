# quanteda: Quantitative Analysis of Textual Data

**quanteda** is an R package for managing and analyzing text, created
and maintained by [Kenneth Benoit](https://kenbenoit.net) and [Kohei
Watanabe](https://blog.koheiw.net/). Its creation was funded by the
European Research Council grant ERC-2011-StG 283794-QUANTESS and its
continued development is supported by the [Quanteda Initiative
CIC](https://quanteda.org).

The package is designed for R users needing to apply natural language
processing to texts, from documents to final analysis. Its capabilities
match or exceed those provided in many end-user software applications,
many of which are expensive and not open source. The package is
therefore of great benefit to researchers, students, and other analysts
with fewer financial resources. While using **quanteda** requires R
programming knowledge, its API is designed to enable powerful, efficient
analysis with a minimum of steps. By emphasizing consistent design,
furthermore, **quanteda** lowers the barriers to learning and using NLP
and quantitative text analysis even for proficient R programmers.

## **quanteda** version 4

The **quanteda** 4.0 is a major release that improves functionality and
performance and further improves function consistency by removing
previously deprecated functions. It also includes significant new
tokeniser rules that makes the default tokeniser smarter than ever
before, with new Unicode and ICU-compliant rules that enable it to work
more consistently with even more languages.

We describe more fully these significant changes in: \* an [article
about the new external pointer tokens
objects](https://quanteda.io/articles/pkgdown/tokens_xptr.html); \* an
[article showing performance
benchmarks](https://quanteda.io/articles/pkgdown/benchmarks_xptr.html)
for the new external pointer tokens objects, as well as some of the
tokeniser improvements in v4; and \* the [changelog for
v4](https://github.com/quanteda/quanteda/blob/master/NEWS.md#quanteda-40)
a full listing of the changes, improvements, and deprecations in v4.

## The **quanteda** family of packages

We completed the trend of splitting **quanteda** into modular packages
with the release of v3. The quanteda family of packages includes the
following:

-   **quanteda**: contains all of the core natural language processing
    and textual data management functions
-   **quanteda.textmodels**: contains all of the text models and
    supporting functions, namely the `textmodel_*()` functions. This was
    split from the main package with the v2 release
-   **quanteda.textstats**: statistics for textual data, namely the
    `textstat_*()` functions, split with the v3 release
-   **quanteda.textplots**: plots for textual data, namely the
    `textplot_*()` functions, split with the v3 release

We are working on additional package releases, available in the meantime
from our GitHub pages:

-   **quanteda.sentiment**: Functions and lexicons for sentiment
    analysis using dictionaries
-   **quanteda.tidy**: Extensions for manipulating document variables in
    core **quanteda** objects using your favourite **tidyverse**
    functions

## How To…

### Install (binaries) from CRAN

The normal way from CRAN, using your R GUI or

    install.packages("quanteda") 

**(New for quanteda v4.0)** For Linux users: Because all installations
on Linux are compiled, Linux users will first need to install the Intel
oneAPI Threading Building Blocks for parallel computing for installation
to work.

To install TBB on Linux:

    # Fedora, CentOS, RHEL
    sudo yum install tbb-devel

    # Debian and Ubuntu
    sudo apt install libtbb-dev

### Compile from source (macOS and Windows)

Because this compiles some C++ and Fortran source code, you will need to
have installed the appropriate compilers to build the development
version.

You will also need to install TBB:

**macOS:**

After installing [Homebrew](https://brew.sh):

    brew install tbb

**Windows:**

Install [RTools](https://cran.r-project.org/bin/windows/Rtools/), which
includes the TBB libraries.

### Use **quanteda**

See the [quick start
guide](https://quanteda.io/articles/quickstart.html) to learn how to use
**quanteda**.

### Get Help

-   Read out documentation at <https://quanteda.io>.
-   Submit a question on the [**quanteda** channel on
    StackOverflow](https://stackoverflow.com/questions/tagged/quanteda).
-   See our [tutorial site](https://tutorials.quanteda.io/).

### Cite the package

Benoit, Kenneth, Kohei Watanabe, Haiyan Wang, Paul Nulty, Adam Obeng,
Stefan Müller, and Akitaka Matsuo. (2018) “[quanteda: An R package for
the quantitative analysis of textual
data](https://www.theoj.org/joss-papers/joss.00774/10.21105.joss.00774.pdf)”.
*Journal of Open Source Software*. 3(30), 774.
<https://doi.org/10.21105/joss.00774>.

For a BibTeX entry, use the output from
`citation(package = "quanteda")`.

### Leave Feedback

If you like **quanteda**, please consider leaving [feedback or a
testimonial here](https://github.com/quanteda/quanteda/issues/461).

### Contribute

Contributions in the form of feedback, comments, code, and bug reports
are most welcome. How to contribute:

-   Fork the source code, modify, and issue a [pull
    request](https://help.github.com/articles/creating-a-pull-request-from-a-fork/)
    through the [project GitHub
    page](https://github.com/quanteda/quanteda). See our [Contributor
    Code of
    Conduct](https://github.com/quanteda/quanteda/blob/master/CONDUCT.md)
    and the all-important **quanteda** [Style
    Guide](https://github.com/quanteda/quanteda/wiki/Style-guide).
-   Issues, bug reports, and wish lists: [File a GitHub
    issue](https://github.com/quanteda/quanteda/issues).
-   Contact [the maintainer](mailto:kbenoit@lse.ac.uk) by email.
