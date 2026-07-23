# Extract a subset of a dfm

Returns document subsets of a dfm that meet certain conditions,
including direct logical operations on docvars (document-level
variables). `dfm_subset` functions identically to
[`subset.data.frame()`](https://rdrr.io/r/base/subset.html), using
non-standard evaluation to evaluate conditions based on the
[docvars](https://quanteda.io/reference/docvars.md) in the dfm.

## Usage

``` r
dfm_subset(
  x,
  subset,
  min_ntoken = NULL,
  max_ntoken = NULL,
  drop_docid = TRUE,
  verbose = quanteda_options("verbose"),
  ...
)
```

## Arguments

- x:

  [dfm](https://quanteda.io/reference/dfm.md) object to be subsetted.

- subset:

  logical expression indicating the documents to keep: missing values
  are taken as false.

- min_ntoken, max_ntoken:

  minimum and maximum lengths of the documents to extract.

- drop_docid:

  if `TRUE`, `docid` for documents are removed as the result of
  subsetting.

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

- ...:

  not used

## Value

[dfm](https://quanteda.io/reference/dfm.md) object, with a subset of
documents (and docvars) selected according to arguments

## Details

To select or subset *features*, see
[`dfm_select()`](https://quanteda.io/reference/dfm_select.md) instead.

## Examples

``` r
corp <- corpus(c(d1 = "a b c d", d2 = "a a b e",
                 d3 = "b b c e", d4 = "e e f a b"),
               docvars = data.frame(grp = c(1, 1, 2, 3)))
dfmat <- dfm(tokens(corp))
# selecting on a docvars condition
dfm_subset(dfmat, grp > 1)
#> Document-feature matrix of: 2 documents, 6 features (41.67% sparse) and 1
#> docvar.
#>     features
#> docs a b c d e f
#>   d3 0 2 1 0 1 0
#>   d4 1 1 0 0 2 1
# selecting on a supplied vector
dfm_subset(dfmat, c(TRUE, FALSE, TRUE, FALSE))
#> Document-feature matrix of: 2 documents, 6 features (41.67% sparse) and 1
#> docvar.
#>     features
#> docs a b c d e f
#>   d1 1 1 1 1 0 0
#>   d3 0 2 1 0 1 0
```
