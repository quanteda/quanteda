# Coercion, checking, and combining functions for tokens objects

Coercion functions to and from
[tokens](https://quanteda.io/reference/tokens.md) objects, checks for
whether an object is a [tokens](https://quanteda.io/reference/tokens.md)
object, and functions to combine
[tokens](https://quanteda.io/reference/tokens.md) objects.

## Usage

``` r
# S3 method for class 'tokens'
as.list(x, ...)

# S3 method for class 'tokens'
as.character(x, use.names = FALSE, ...)

is.tokens(x)

as.tensor(x, ...)

# S3 method for class 'tokens'
as.tensor(x, length = NULL, extract = NULL, ...)

# S3 method for class 'tokens'
as.matrix(x, length = NULL, extract = NULL, drop = TRUE, ...)

as.tokens(x, concatenator = "_", ...)

# S3 method for class 'spacyr_parsed'
as.tokens(
  x,
  concatenator = "/",
  include_pos = c("none", "pos", "tag"),
  use_lemma = FALSE,
  ...
)

is.tokens(x)
```

## Arguments

- x:

  object to be coerced or checked

- ...:

  additional arguments used by specific methods. For
  [c.tokens](https://quanteda.io/reference/tokens-class.md), these are
  the [tokens](https://quanteda.io/reference/tokens.md) objects to be
  concatenated.

- use.names:

  logical; preserve names if `TRUE`. For `as.character` and `unlist`
  only.

- length:

  optional integer specifying the maximum length (number of positions)
  for the tensor. If `NULL` (default), the length is inferred from the
  maximum token position across all documents.

- extract:

  indices for documents to be extracted from `x`.

- drop:

  if `TRUE` returns a vector instead of a matrix with only one row.

- concatenator:

  character; the concatenation character that will connect the tokens
  making up a multi-token sequence.

- include_pos:

  character; whether and which part-of-speech tag to use: `"none"` do
  not use any part of speech indicator, `"pos"` use the `pos` variable,
  `"tag"` use the `tag` variable. The POS will be added to the token
  after `"concatenator"`.

- use_lemma:

  logical; if `TRUE`, use the lemma rather than the raw token

## Value

`as.list` returns a simple list of characters from a
[tokens](https://quanteda.io/reference/tokens.md) object.

`as.character` returns a character vector from a
[tokens](https://quanteda.io/reference/tokens.md) object.

`is.tokens` returns `TRUE` if the object is of class tokens, `FALSE`
otherwise.

`as.tensor` returns a tensor from a
[tokens](https://quanteda.io/reference/tokens.md) object, compatible
with the torch package. Each document is represented as a row, and token
positions as columns. The integer token IDs in the resulting tensor are
shifted by one to comply with 1-based indexing in R.

`as.tokens` returns a quanteda
[tokens](https://quanteda.io/reference/tokens.md) object.

`is.tokens` returns `TRUE` if the object is of class tokens, `FALSE`
otherwise.

## Details

The `concatenator` is used to automatically generate dictionary values
for multi-word expressions in
[`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md) and
[`dfm_lookup()`](https://quanteda.io/reference/dfm_lookup.md). The
underscore character is commonly used to join elements of multi-word
expressions (e.g. "piece_of_cake", "New_York"), but other characters
(e.g. whitespace " " or a hyphen "-") can also be used. In those cases,
users have to tell the system what is the concatenator in your tokens so
that the conversion knows to treat this character as the inter-word
delimiter, when reading in the elements that will become the tokens.

## Examples

``` r
if (FALSE) { # \dontrun{
library(torch)
toks <- tokens(c(doc1 = "a b c d e f g",
                 doc2 = "a b c g",
                 doc3 = ""))
as.tensor(toks)
} # }

# create tokens object from list of characters with custom concatenator
dict <- dictionary(list(country = "United States",
                   sea = c("Atlantic Ocean", "Pacific Ocean")))
lis <- list(c("The", "United-States", "has", "the", "Atlantic-Ocean",
              "and", "the", "Pacific-Ocean", "."))
toks <- as.tokens(lis, concatenator = "-")
tokens_lookup(toks, dict)
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "country" "sea"     "sea"    
#> 
```
