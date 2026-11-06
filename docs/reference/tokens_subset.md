# Extract a subset of a tokens

Returns document subsets of a tokens that meet certain conditions,
including direct logical operations on docvars (document-level
variables). `tokens_subset()` functions identically to
[`subset.data.frame()`](https://rdrr.io/r/base/subset.html), using
non-standard evaluation to evaluate conditions based on the
[docvars](https://quanteda.io/reference/docvars.md) in the tokens.

## Usage

``` r
tokens_subset(
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

  [tokens](https://quanteda.io/reference/tokens.md) object to be
  subsetted.

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

[tokens](https://quanteda.io/reference/tokens.md) object, with a subset
of documents (and docvars) selected according to arguments

## See also

[`subset.data.frame()`](https://rdrr.io/r/base/subset.html)

## Examples

``` r
corp <- corpus(c(d1 = "a b c d", d2 = "a a b e",
                 d3 = "b b c e", d4 = "e e f a b"),
                 docvars = data.frame(grp = c(1, 1, 2, 3)))
toks <- tokens(corp)
# selecting on a docvars condition
tokens_subset(toks, grp > 1)
#> Tokens consisting of 2 documents and 1 docvar.
#> d3 :
#> [1] "b" "b" "c" "e"
#> 
#> d4 :
#> [1] "e" "e" "f" "a" "b"
#> 
# selecting on a supplied vector
tokens_subset(toks, c(TRUE, FALSE, TRUE, FALSE))
#> Tokens consisting of 2 documents and 1 docvar.
#> d1 :
#> [1] "a" "b" "c" "d"
#> 
#> d3 :
#> [1] "b" "b" "c" "e"
#> 
```
