
# quanteda: Quantitative Analysis of Textual Data

## About

An R package for managing and analyzing text, created by [Kenneth
Benoit](http://kenbenoit.net) in collaboration with a team of core
[contributors](https://github.com/quanteda/quanteda/graphs/contributors):
[Kohei Watanabe](https://github.com/koheiw), [Haiyan
Wang](https://github.com/HaiyanLW), [Paul
Nulty](https://github.com/pnulty), [Adam
Obeng](https://github.com/adamobeng), [Stefan
Müller](http://muellerstefan.net), and [Akitaka
Matsuo](http://amatsuo.net). Supported by the European Research Council
grant ERC-2011-StG 283794-QUANTESS.

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
gfortran to build).

## How to Use

See the [quick start
guide](http://docs.quanteda.io/articles/quickstart.html) to learn how to
use **quanteda**.

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
