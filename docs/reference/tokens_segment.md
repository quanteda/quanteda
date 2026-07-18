# Segment tokens object by patterns

Segment tokens by splitting on a pattern match. This is useful for
breaking the tokenized texts into smaller document units, based on a
regular pattern or a user-supplied annotation. While it normally makes
more sense to do this at the corpus level (see
[`corpus_segment()`](https://quanteda.io/reference/corpus_segment.md)),
`tokens_segment` provides the option to perform this operation on
tokens.

## Usage

``` r
tokens_segment(
  x,
  pattern,
  valuetype = c("glob", "regex", "fixed"),
  case_insensitive = TRUE,
  extract_pattern = FALSE,
  pattern_position = c("before", "after"),
  use_docvars = TRUE,
  verbose = quanteda_options("verbose")
)
```

## Arguments

- x:

  [tokens](https://quanteda.io/reference/tokens.md) object whose token
  elements will be segmented

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

- case_insensitive:

  logical; if `TRUE`, ignore case when matching a `pattern` or
  [dictionary](https://quanteda.io/reference/dictionary.md) values

- extract_pattern:

  remove matched patterns from the texts and save in
  [docvars](https://quanteda.io/reference/docvars.md), if `TRUE`

- pattern_position:

  either `"before"` or `"after"`, depending on whether the pattern
  precedes the text (as with a tag) or follows the text (as with
  punctuation delimiters)

- use_docvars:

  if `TRUE`, repeat the docvar values for each segmented text; if
  `FALSE`, drop the docvars in the segmented corpus. Dropping the
  docvars might be useful in order to conserve space or if these are not
  desired for the segmented corpus.

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## Value

`tokens_segment` returns a
[tokens](https://quanteda.io/reference/tokens.md) object whose documents
have been split by patterns

## Examples

``` r
txts <- "Fellow citizens, I am again called upon by the voice of my country to
execute the functions of its Chief Magistrate. When the occasion proper for
it shall arrive, I shall endeavor to express the high sense I entertain of
this distinguished honor."
toks <- tokens(txts)

# split by any punctuation
tokens_segment(toks, "^\\p{Sterm}$", valuetype = "regex",
               extract_pattern = TRUE,
               pattern_position = "after")
#> Tokens consisting of 2 documents and 1 docvar.
#> text1.1 :
#>  [1] "Fellow"   "citizens" ","        "I"        "am"       "again"   
#>  [7] "called"   "upon"     "by"       "the"      "voice"    "of"      
#> [ ... and 10 more ]
#> 
#> text1.2 :
#>  [1] "When"     "the"      "occasion" "proper"   "for"      "it"      
#>  [7] "shall"    "arrive"   ","        "I"        "shall"    "endeavor"
#> [ ... and 11 more ]
#> 
tokens_segment(toks, c(".", "?", "!"), valuetype = "fixed",
               extract_pattern = TRUE,
               pattern_position = "after")
#> Tokens consisting of 2 documents and 1 docvar.
#> text1.1 :
#>  [1] "Fellow"   "citizens" ","        "I"        "am"       "again"   
#>  [7] "called"   "upon"     "by"       "the"      "voice"    "of"      
#> [ ... and 10 more ]
#> 
#> text1.2 :
#>  [1] "When"     "the"      "occasion" "proper"   "for"      "it"      
#>  [7] "shall"    "arrive"   ","        "I"        "shall"    "endeavor"
#> [ ... and 11 more ]
#> 
```
