# Convert token sequences into compound tokens

Replace multi-token sequences with a multi-word, or "compound" token.
The resulting compound tokens will represent a phrase or multi-word
expression, concatenated with `concatenator` (by default, the "`_`"
character) to form a single "token". This ensures that the sequences
will be processed subsequently as single tokens, for instance in
constructing a [dfm](https://quanteda.io/reference/dfm.md).

## Usage

``` r
tokens_compound(
  x,
  pattern,
  valuetype = c("glob", "regex", "fixed"),
  concatenator = concat(x),
  window = 0L,
  case_insensitive = TRUE,
  join = TRUE,
  keep_unigrams = FALSE,
  apply_if = NULL,
  verbose = quanteda_options("verbose")
)
```

## Arguments

- x:

  an input [tokens](https://quanteda.io/reference/tokens.md) object

- pattern:

  a character vector, list of character vectors,
  [dictionary](https://quanteda.io/reference/dictionary.md), or
  collocations object. See
  [pattern](https://quanteda.io/reference/pattern.md) for details.

- valuetype:

  the type of pattern matching: `"glob"` for "glob"-style wildcard
  expressions; `"regex"` for regular expressions; or `"fixed"` for exact
  matching. See [valuetype](https://quanteda.io/reference/valuetype.md)
  for details.

- concatenator:

  character; the concatenation character that will connect the tokens
  making up a multi-token sequence.

- window:

  integer; a vector of length 1 or 2 that specifies size of the window
  of tokens adjacent to `pattern` that will be compounded with matches
  to `pattern`. The window can be asymmetric if two elements are
  specified, with the first giving the window size before `pattern` and
  the second the window size after. If paddings (empty `""` tokens) are
  found, window will be shrunk to exclude them.

- case_insensitive:

  logical; if `TRUE`, ignore case when matching a `pattern` or
  [dictionary](https://quanteda.io/reference/dictionary.md) values

- join:

  logical; if `TRUE`, join overlapping compounds into a single compound;
  otherwise, form these separately. See examples.

- keep_unigrams:

  if `TRUE`, keep the original tokens.

- apply_if:

  logical vector of length `ndoc(x)`; documents are modified only when
  corresponding values are `TRUE`, others are left unchanged.

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## Value

A [tokens](https://quanteda.io/reference/tokens.md) object in which the
token sequences matching `pattern` have been replaced by new compounded
"tokens" joined by the concatenator.

## Note

Patterns to be compounded (naturally) consist of multi-word sequences,
and how these are expected in `pattern` is very specific. If the
elements to be compounded are supplied as space-delimited elements of a
character vector, wrap the vector in
[`phrase()`](https://quanteda.io/reference/phrase.md). If the elements
to be compounded are separate elements of a character vector, supply it
as a list where each list element is the sequence of character elements.

See the examples below.

## Examples

``` r
txt <- "The United Kingdom is leaving the European Union."
toks <- tokens(txt, remove_punct = TRUE)

# character vector - not compounded
tokens_compound(toks, c("United", "Kingdom", "European", "Union"))
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "The"      "United"   "Kingdom"  "is"       "leaving"  "the"      "European"
#> [8] "Union"   
#> 

# elements separated by spaces - not compounded
tokens_compound(toks, c("United Kingdom", "European Union"))
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "The"      "United"   "Kingdom"  "is"       "leaving"  "the"      "European"
#> [8] "Union"   
#> 

# list of characters - is compounded
tokens_compound(toks, list(c("United", "Kingdom"), c("European", "Union")))
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "The"            "United_Kingdom" "is"             "leaving"       
#> [5] "the"            "European_Union"
#> 

# elements separated by spaces, wrapped in phrase() - is compounded
tokens_compound(toks, phrase(c("United Kingdom", "European Union")))
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "The"            "United_Kingdom" "is"             "leaving"       
#> [5] "the"            "European_Union"
#> 

# supplied as values in a dictionary (same as list) - is compounded
# (keys do not matter)
tokens_compound(toks, dictionary(list(key1 = "United Kingdom",
                                      key2 = "European Union")))
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "The"            "United_Kingdom" "is"             "leaving"       
#> [5] "the"            "European_Union"
#> 
# pattern as dictionaries with glob matches
tokens_compound(toks, dictionary(list(key1 = c("U* K*"))), valuetype = "glob")
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "The"            "United_Kingdom" "is"             "leaving"       
#> [5] "the"            "European"       "Union"         
#> 

# note the differences caused by join = FALSE
compounds <- list(c("the", "European"), c("European", "Union"))
tokens_compound(toks, pattern = compounds, join = TRUE)
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "The"                "United"             "Kingdom"           
#> [4] "is"                 "leaving"            "the_European_Union"
#> 
tokens_compound(toks, pattern = compounds, join = FALSE)
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "The"            "United"         "Kingdom"        "is"            
#> [5] "leaving"        "the_European"   "European_Union"
#> 

# use window to form ngrams
tokens_remove(toks, pattern = stopwords("en")) |>
    tokens_compound(pattern = "leav*", join = FALSE, window = c(0, 3))
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "United"                 "Kingdom"                "leaving_European_Union"
#> 
```
