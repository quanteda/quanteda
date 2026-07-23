# Combine documents in a dfm by a grouping variable

Combine documents in a [dfm](https://quanteda.io/reference/dfm.md) by a
grouping variable, by summing the cell frequencies within group and
creating new "documents" with the group labels.

## Usage

``` r
dfm_group(
  x,
  groups = docid(x),
  fill = FALSE,
  force = FALSE,
  verbose = quanteda_options("verbose")
)
```

## Arguments

- x:

  a [dfm](https://quanteda.io/reference/dfm.md)

- groups:

  grouping variable for sampling, equal in length to the number of
  documents. This will be evaluated in the docvars data.frame, so that
  docvars may be referred to by name without quoting. This also changes
  previous behaviours for `groups`. See
  `news(Version >= "3.0", package = "quanteda")` for details.

- fill:

  logical; if `TRUE` and `groups` is a factor, then use all levels of
  the factor when forming the new documents of the grouped object. This
  will result in a new "document" with empty content for levels not
  observed, but for which an empty document may be needed. If `groups`
  is a factor of dates, for instance, then `fill = TRUE` ensures that
  the new object will consist of one new "document" by date, regardless
  of whether any documents previously existed with that date. Has no
  effect if the `groups` variable(s) are not factors.

- force:

  logical; if `TRUE`, group by summing existing counts, even if the dfm
  has been weighted. This can result in invalid sums, such as adding log
  counts (when a dfm has been weighted by `"logcount"` for instance
  using [`dfm_weight()`](https://quanteda.io/reference/dfm_weight.md)).
  Not needed when the term weight schemes "count" and "prop".

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## Value

`dfm_group` returns a [dfm](https://quanteda.io/reference/dfm.md) whose
documents are equal to the unique group combinations, and whose cell
values are the sums of the previous values summed by group.
Document-level variables that have no variation within groups are saved
in [docvars](https://quanteda.io/reference/docvars.md). Document-level
variables that are lists are dropped from grouping, even when these
exhibit no variation within groups.

## Examples

``` r
corp <- corpus(c("a a b", "a b c c", "a c d d", "a c c d"),
               docvars = data.frame(grp = c("grp1", "grp1", "grp2", "grp2")))
dfmat <- dfm(tokens(corp))
dfm_group(dfmat, groups = grp)
#> Document-feature matrix of: 2 documents, 4 features (25.00% sparse) and 1
#> docvar.
#>       features
#> docs   a b c d
#>   grp1 3 2 2 0
#>   grp2 2 0 3 3
dfm_group(dfmat, groups = c(1, 1, 2, 2))
#> Document-feature matrix of: 2 documents, 4 features (25.00% sparse) and 1
#> docvar.
#>     features
#> docs a b c d
#>    1 3 2 2 0
#>    2 2 0 3 3

# with fill = TRUE
dfm_group(dfmat, fill = TRUE,
          groups = factor(c("A", "A", "B", "C"), levels = LETTERS[1:4]))
#> Document-feature matrix of: 4 documents, 4 features (43.75% sparse) and 1
#> docvar.
#>     features
#> docs a b c d
#>    A 3 2 2 0
#>    B 1 0 1 2
#>    C 1 0 2 1
#>    D 0 0 0 0
```
