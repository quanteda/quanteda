# Match the tokens IDs with given types

Match the token types and IDs of multiple
[tokens](https://quanteda.io/reference/tokens.md) objects using a
character vector.

## Usage

``` r
tokens_match(x, types, verbose = quanteda_options("verbose"))
```

## Arguments

- x:

  the [tokens](https://quanteda.io/reference/tokens.md) object.

- types:

  character vector for the token types to be matched in the resulting
  [tokens](https://quanteda.io/reference/tokens.md). Tokens not included
  in `types` are all removed.

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## See also

dfm_match

## Examples

``` r
txt <- c("This is text one", "The text two", "This is text three")
(toks1 <- tokens(txt[1:2]))
#> Tokens consisting of 2 documents.
#> text1 :
#> [1] "This" "is"   "text" "one" 
#> 
#> text2 :
#> [1] "The"  "text" "two" 
#> 
(toks2 <- tokens(txt[2:3]))
#> Tokens consisting of 2 documents.
#> text1 :
#> [1] "The"  "text" "two" 
#> 
#> text2 :
#> [1] "This"  "is"    "text"  "three"
#> 
(toks3 <- tokens_match(toks1, types(toks2)))
#> Tokens consisting of 2 documents.
#> text1 :
#> [1] "This" "is"   "text"
#> 
#> text2 :
#> [1] "The"  "text" "two" 
#> 
identical(types(toks2), types(toks3))
#> [1] TRUE
```
