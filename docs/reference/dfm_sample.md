# Randomly sample documents from a dfm

Take a random sample of documents of the specified size from a dfm, with
or without replacement, optionally by grouping variables or with
probability weights.

## Usage

``` r
dfm_sample(
  x,
  size = NULL,
  replace = FALSE,
  prob = NULL,
  by = NULL,
  verbose = quanteda_options("verbose")
)
```

## Arguments

- x:

  the [dfm](https://quanteda.io/reference/dfm.md) object whose documents
  will be sampled

- size:

  a positive number, the number of documents to select; when used with
  `by`, the number to select from each group or a vector equal in length
  to the number of groups defining the samples to be chosen in each
  category of `by`. By defining a size larger than the number of
  documents, it is possible to oversample when `replace = TRUE`.

- replace:

  if `TRUE`, sample with replacement

- prob:

  a vector of probability weights for obtaining the elements of the
  vector being sampled. May not be applied when `by` is used.

- by:

  optional grouping variable for sampling. This will be evaluated in the
  docvars data.frame, so that docvars may be referred to by name without
  quoting. This also changes previous behaviours for `by`. See
  `news(Version >= "2.9", package = "quanteda")` for details.

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## Value

a [dfm](https://quanteda.io/reference/dfm.md) object (re)sampled on the
documents, containing the document variables for the documents sampled.

## See also

[sample](https://rdrr.io/r/base/sample.html)

## Examples

``` r
set.seed(10)
dfmat <- dfm(tokens(c("a b c c d", "a a c c d d d", "a b b c")))
dfmat
#> Document-feature matrix of: 3 documents, 4 features (16.67% sparse) and 0
#> docvars.
#>        features
#> docs    a b c d
#>   text1 1 1 2 1
#>   text2 2 0 2 3
#>   text3 1 2 1 0
dfm_sample(dfmat)
#> Document-feature matrix of: 3 documents, 4 features (16.67% sparse) and 0
#> docvars.
#>        features
#> docs    a b c d
#>   text3 1 2 1 0
#>   text1 1 1 2 1
#>   text2 2 0 2 3
dfm_sample(dfmat, replace = TRUE)
#> Document-feature matrix of: 3 documents, 4 features (25.00% sparse) and 0
#> docvars.
#>          features
#> docs      a b c d
#>   text3.1 1 2 1 0
#>   text2.1 2 0 2 3
#>   text3.2 1 2 1 0

# by groups
dfmat <- dfm(tokens(data_corpus_inaugural[50:58]))
dfm_sample(dfmat, by = Party, size = 2)
#> Document-feature matrix of: 4 documents, 2,999 features (74.71% sparse) and 4
#> docvars.
#>             features
#> docs         senator mathias   , chief justice burger vice president bush
#>   2009-Obama       0       0 130     0       0      0    0         1    1
#>   2013-Obama       0       0  99     1       2      0    1         2    0
#>   1989-Bush        2       0 166     1       2      0    1         6    0
#>   2001-Bush        0       0 110     0       3      0    1         3    0
#>             features
#> docs         speaker
#>   2009-Obama       0
#>   2013-Obama       0
#>   1989-Bush        3
#>   2001-Bush        0
#> [ reached max_nfeat ... 2,989 more features ]
```
