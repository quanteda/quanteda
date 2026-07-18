# Split tokens by a separator pattern

Replaces tokens by multiple replacements consisting of elements split by
a separator pattern, with the option of retaining the separator. This
function effectively reverses the operation of
[`tokens_compound()`](https://quanteda.io/reference/tokens_compound.md).

## Usage

``` r
tokens_split(
  x,
  separator = " ",
  valuetype = c("fixed", "regex"),
  remove_separator = TRUE,
  apply_if = NULL,
  verbose = quanteda_options("verbose")
)
```

## Arguments

- x:

  a [tokens](https://quanteda.io/reference/tokens.md) object

- separator:

  a single-character pattern match by which tokens are separated

- valuetype:

  the type of pattern matching: `"glob"` for "glob"-style wildcard
  expressions; `"regex"` for regular expressions; or `"fixed"` for exact
  matching. See [valuetype](https://quanteda.io/reference/valuetype.md)
  for details.

- remove_separator:

  if `TRUE`, remove separator from new tokens

- apply_if:

  logical vector of length `ndoc(x)`; documents are modified only when
  corresponding values are `TRUE`, others are left unchanged.

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## Examples

``` r
# undo tokens_compound()
toks1 <- tokens("pork barrel is an idiomatic multi-word expression")
tokens_compound(toks1, phrase("pork barrel"))
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "pork_barrel" "is"          "an"          "idiomatic"   "multi-word" 
#> [6] "expression" 
#> 
tokens_compound(toks1, phrase("pork barrel")) |>
    tokens_split(separator = "_")
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "pork"       "barrel"     "is"         "an"         "idiomatic" 
#> [6] "multi-word" "expression"
#> 

# similar to tokens(x, remove_hyphen = TRUE) but post-tokenization
toks2 <- tokens("UK-EU negotiation is not going anywhere as of 2018-12-24.")
tokens_split(toks2, separator = "-", remove_separator = FALSE)
#> Tokens consisting of 1 document.
#> text1 :
#>  [1] "UK"          "-"           "EU"          "negotiation" "is"         
#>  [6] "not"         "going"       "anywhere"    "as"          "of"         
#> [11] "2018"        "-"          
#> [ ... and 4 more ]
#> 
```
