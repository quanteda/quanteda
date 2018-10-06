quanteda: Quantitative Analysis of Textual Data
===============================================

**quanteda** is an R package for managing and analyzing textual data
developed by [Kenneth Benoit](http://kenbenoit.net) and other
contributors. Its initial development was supported by the European
Research Council grant ERC-2011-StG 283794-QUANTESS.

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

How to Install
--------------

The normal way from CRAN, using your R GUI or

    install.packages("quanteda") 

Or for the latest development version:

    # devtools package required to install quanteda from Github 
    devtools::install_github("quanteda/quanteda") 

Because this compiles some C++ and Fortran source code, you will need to
have installed the appropriate compilers.

**If you are using a Windows platform**, this means you will need also
to install the [Rtools](https://CRAN.R-project.org/bin/windows/Rtools/)
software available from CRAN.

**If you are using macOS**, you should install the [macOS
tools](https://cran.r-project.org/bin/macosx/tools/), namely the Clang
6.x compiler and the GNU Fortran compiler (as **quanteda** requires
gfortran to build).

System Requirements
-------------------

**quanteda** is cross-platform but we recommend MacOS or Linux as an
operating system for their better handling of Unicode. RAM depends on
the size and the structure of the textual data to analyze. Usually, a
text file of 100MB on disk takes 500MB to 1GB on memory as a tokens
object (short texts require more memory than long texts when the total
numbers of words are the same).

<table>
<thead>
<tr class="header">
<th></th>
<th style="text-align: center;">Minimum</th>
<th style="text-align: center;">Recommended</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>OS</td>
<td style="text-align: center;">Windows/MacOS/Linux</td>
<td style="text-align: center;">MacOS/Linux</td>
</tr>
<tr class="even">
<td>CPU</td>
<td style="text-align: center;">1 core</td>
<td style="text-align: center;">4 cores or more</td>
</tr>
<tr class="odd">
<td>RAM</td>
<td style="text-align: center;">2GB</td>
<td style="text-align: center;">8GB more more</td>
</tr>
<tr class="even">
<td>IDE</td>
<td style="text-align: center;"></td>
<td style="text-align: center;">R Studio</td>
</tr>
</tbody>
</table>

How to Use
----------

See the [quick start
guide](https://quanteda.io/articles/quickstart.html) to learn how to use
**quanteda**.

How to cite
-----------

Benoit, Kenneth, Kohei Watanabe, Haiyan Wang, Paul Nulty, Adam Obeng,
Stefan Müller, and Akitaka Matsuo. (2018) “[quanteda: An R package for
the quantitative analysis of textual
data](https://www.theoj.org/joss-papers/joss.00774/10.21105.joss.00774.pdf)”.
*Journal of Open Source Software*. 3(30), 774.
<https://doi.org/10.21105/joss.00774>.

For a BibTeX entry, use the output from
`citation(package = "quanteda")`.

Leaving Feedback
----------------

If you like **quanteda**, please consider leaving [feedback or a
testimonial here](https://github.com/quanteda/quanteda/issues/461).

Contributing
------------

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
-   Usage questions: Submit a question on the [**quanteda** channel on
    StackOverflow](https://stackoverflow.com/questions/tagged/quanteda).
-   Contact [the maintainer](mailto:kbenoit@lse.ac.uk) by email.
