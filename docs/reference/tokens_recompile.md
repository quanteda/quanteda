# Remove redundant or unused tokens IDs

This function reassign tokens IDs after changes in removal or
transformation of tokens. It update token IDs to ensure that they are
unique and dense. Usually, this function is only applied to
[tokens_xptr](https://quanteda.io/reference/tokens_xptr.md) objects at
the end of text pre-processing.

## Usage

``` r
tokens_recompile(x, force = FALSE)
```

## Arguments

- x:

  the [tokens_xptr](https://quanteda.io/reference/tokens_xptr.md)
  object.

- force:

  if `TRUE`, update tokens IDs even when it may not be necessary.

## Examples

``` r
toks <- tokens(c(one = "a b c d A B C D",
                 two = "A B C d"), xptr = TRUE)
toks <- tokens_tolower(toks)
types(toks)
#> [1] "a" "b" "c" "d"
types(tokens_recompile(toks))
#> [1] "a" "b" "c" "d"
```
