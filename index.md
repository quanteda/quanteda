# quanteda: Quantitative Analysis of Textual Data

**quanteda** is an R package for managing and analyzing textual data
developed by [Kenneth Benoit](http://kenbenoit.net), [Kohei
Watanabe](https://blog.koheiw.net/), and other contributors. Its initial
development was supported by the European Research Council grant
ERC-2011-StG 283794-QUANTESS.

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

## **quanteda** version 3: New major release

**quanteda** 3.0 is a major release that improves functionality,
completes the modularisation of the package begun in v2.0, further
improves function consistency by removing previously deprecated
functions, and enhances workflow stability and consistency by
deprecating some shortcut steps built into some functions.

See
<https://github.com/quanteda/quanteda/blob/master/NEWS.md#quanteda-30>
for a full list of the changes.

## The **quanteda** family of packages

As of v3.0, we have continued our trend of splitting **quanteda** into
modular packages. These are now the following:

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

and more to come.

## How To…

### How to Install

The normal way from CRAN, using your R GUI or

    install.packages("quanteda") 

Or for the latest development version:

    # devtools package required to install quanteda from Github 
    remotes::install_github("quanteda/quanteda") 

Because this compiles some C++ and Fortran source code, you will need to
have installed the appropriate compilers to build the development
version.

### How to Use

See the [quick start
guide](https://quanteda.io/articles/pkgdown/quickstart.html) to learn
how to use **quanteda**.

### How to Get Help

-   Read out documentation at <https://quanteda.io>.
-   Submit a question on the [**quanteda** channel on
    StackOverflow](https://stackoverflow.com/questions/tagged/quanteda).
-   See our [tutorial site](https://tutorials.quanteda.io/).

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
