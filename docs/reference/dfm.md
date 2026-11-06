# Create a document-feature matrix

Construct a sparse document-feature matrix from a
[tokens](https://quanteda.io/reference/tokens.md) or dfm object.

## Usage

``` r
dfm(
  x,
  tolower = TRUE,
  remove_padding = FALSE,
  trim = TRUE,
  verbose = quanteda_options("verbose"),
  ...
)
```

## Arguments

- x:

  a [tokens](https://quanteda.io/reference/tokens.md) or dfm object.

- tolower:

  convert all features to lowercase.

- remove_padding:

  logical; if `TRUE`, remove the "pads" left as empty tokens after
  calling [`tokens()`](https://quanteda.io/reference/tokens.md) or
  [`tokens_remove()`](https://quanteda.io/reference/tokens_select.md)
  with `padding = TRUE`.

- trim:

  logical; if `TRUE`, remove columns for features with all zeros. This
  is always `FALSE` when `x` records dictionary keys.

- verbose:

  display messages if `TRUE`.

- ...:

  not used.

## Value

a [dfm](https://quanteda.io/reference/dfm-class.md) object

## Changes in version 3

In quanteda v4, many convenience functions formerly available in `dfm()`
were removed.

## See also

[`as.dfm()`](https://quanteda.io/reference/as.dfm.md),
[`dfm_select()`](https://quanteda.io/reference/dfm_select.md),
[dfm](https://quanteda.io/reference/dfm-class.md)

## Examples

``` r
## for a corpus
toks <- data_corpus_inaugural |>
  corpus_subset(Year > 1980) |>
  tokens()
dfm(toks)
#> Document-feature matrix of: 12 documents, 3,756 features (79.89% sparse) and 4
#> docvars.
#>               features
#> docs           senator hatfield   , mr   . chief justice president vice bush
#>   1981-Reagan        2        1 174  3 130     1       1         5    2    1
#>   1985-Reagan        4        0 177  0 124     1       1         3    1    1
#>   1989-Bush          2        0 166  6 142     1       2         6    1    0
#>   1993-Clinton       0        0 139  0  81     0       0         2    0    1
#>   1997-Clinton       0        0 131  0 108     0       1         1    0    0
#>   2001-Bush          0        0 110  0  96     0       3         3    1    0
#> [ reached max_ndoc ... 6 more documents, reached max_nfeat ... 3,746 more
#> features ]

# removal options
toks <- tokens(c("a b c", "A B C D")) |>
    tokens_remove("b", padding = TRUE)
toks
#> Tokens consisting of 2 documents.
#> text1 :
#> [1] "a" ""  "c"
#> 
#> text2 :
#> [1] "A" ""  "C" "D"
#> 
dfm(toks)
#> Document-feature matrix of: 2 documents, 4 features (12.50% sparse) and 0
#> docvars.
#>        features
#> docs      a c d
#>   text1 1 1 1 0
#>   text2 1 1 1 1
dfm(toks) |>
 dfm_remove(pattern = "") # remove "pads"
#> Document-feature matrix of: 2 documents, 3 features (16.67% sparse) and 0
#> docvars.
#>        features
#> docs    a c d
#>   text1 1 1 0
#>   text2 1 1 1

# preserving case
dfm(toks, tolower = FALSE)
#> Document-feature matrix of: 2 documents, 6 features (41.67% sparse) and 0
#> docvars.
#>        features
#> docs      a c A C D
#>   text1 1 1 1 0 0 0
#>   text2 1 0 0 1 1 1
```
