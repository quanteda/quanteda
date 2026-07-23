# Convert the case of tokens

`tokens_tolower()` and `tokens_toupper()` convert the features of a
[tokens](https://quanteda.io/reference/tokens.md) object and re-index
the types.

## Usage

``` r
tokens_tolower(x, keep_acronyms = FALSE, verbose = quanteda_options("verbose"))

tokens_toupper(x, verbose = quanteda_options("verbose"))
```

## Arguments

- x:

  the input object whose character/tokens/feature elements will be
  case-converted

- keep_acronyms:

  logical; if `TRUE`, do not lowercase any all-uppercase words (applies
  only to `*_tolower()` functions)

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## Examples

``` r
# for a document-feature matrix
toks <- tokens(c(txt1 = "b A A", txt2 = "C C a b B"))
tokens_tolower(toks)
#> Tokens consisting of 2 documents.
#> txt1 :
#> [1] "b" "a" "a"
#> 
#> txt2 :
#> [1] "c" "c" "a" "b" "b"
#> 
tokens_toupper(toks)
#> Tokens consisting of 2 documents.
#> txt1 :
#> [1] "B" "A" "A"
#> 
#> txt2 :
#> [1] "C" "C" "A" "B" "B"
#> 
```
