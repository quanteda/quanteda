# Select or remove tokens from a tokens object

These function select or discard tokens from a
[tokens](https://quanteda.io/reference/tokens.md) object. For
convenience, the functions `tokens_remove` and `tokens_keep` are defined
as shortcuts for `tokens_select(x, pattern, selection = "remove")` and
`tokens_select(x, pattern, selection = "keep")`, respectively. The most
common usage for `tokens_remove` will be to eliminate stop words from a
text or text-based object, while the most common use of `tokens_select`
will be to select tokens with only positive pattern matches from a list
of regular expressions, including a dictionary. `startpos` and `endpos`
determine the positions of tokens searched for `pattern` and areas
affected are expanded by `window`.

## Usage

``` r
tokens_select(
  x,
  pattern,
  selection = c("keep", "remove"),
  valuetype = c("glob", "regex", "fixed"),
  case_insensitive = TRUE,
  padding = FALSE,
  window = 0,
  min_nchar = NULL,
  max_nchar = NULL,
  startpos = 1L,
  endpos = -1L,
  apply_if = NULL,
  verbose = quanteda_options("verbose")
)

tokens_remove(x, ...)

tokens_keep(x, ...)
```

## Arguments

- x:

  [tokens](https://quanteda.io/reference/tokens.md) object whose token
  elements will be removed or kept

- pattern:

  a character vector, list of character vectors,
  [dictionary](https://quanteda.io/reference/dictionary.md), or
  collocations object. See
  [pattern](https://quanteda.io/reference/pattern.md) for details.

- selection:

  whether to `"keep"` or `"remove"` the tokens matching `pattern`

- valuetype:

  the type of pattern matching: `"glob"` for "glob"-style wildcard
  expressions; `"regex"` for regular expressions; or `"fixed"` for exact
  matching. See [valuetype](https://quanteda.io/reference/valuetype.md)
  for details.

- case_insensitive:

  logical; if `TRUE`, ignore case when matching a `pattern` or
  [dictionary](https://quanteda.io/reference/dictionary.md) values

- padding:

  if `TRUE`, leave an empty string where the removed tokens previously
  existed. This is useful if a positional match is needed between the
  pre- and post-selected tokens, for instance if a window of adjacency
  needs to be computed.

- window:

  integer of length 1 or 2; the size of the window of tokens adjacent to
  `pattern` that will be selected. The window is symmetric unless a
  vector of two elements is supplied, in which case the first element
  will be the token length of the window before `pattern`, and the
  second will be the token length of the window after `pattern`. The
  default is `0`, meaning that only the pattern matched token(s) are
  selected, with no adjacent terms.

  Terms from overlapping windows are never double-counted, but simply
  returned in the pattern match. This is because `tokens_select` never
  redefines the document units; for this, see
  [`kwic()`](https://quanteda.io/reference/kwic.md).

- min_nchar, max_nchar:

  optional numerics specifying the minimum and maximum length in
  characters for tokens to be removed or kept; defaults are `NULL` for
  no limits. These are applied after (and hence, in addition to) any
  selection based on pattern matches.

- startpos, endpos:

  integer; position of tokens in documents where pattern matching starts
  and ends, where 1 is the first token in a document. For negative
  indexes, counting starts at the ending token of the document, so that
  -1 denotes the last token in the document, -2 the second to last, etc.
  When the length of the vector is equal to `ndoc`, tokens in
  corresponding positions will be selected; when it is less than `ndoc`,
  values are repeated to make them equal in length.

- apply_if:

  logical vector of length `ndoc(x)`; documents are modified only when
  corresponding values are `TRUE`, others are left unchanged.

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

- ...:

  additional arguments passed by `tokens_remove` and `tokens_keep` to
  `tokens_select`. Cannot include `selection`.

## Value

a [tokens](https://quanteda.io/reference/tokens.md) object with tokens
selected or removed based on their match to `pattern`

## Examples

``` r
## tokens_select with simple examples
toks <- as.tokens(list(letters, LETTERS))
tokens_select(toks, c("b", "e", "f"), selection = "keep", padding = FALSE)
#> Tokens consisting of 2 documents.
#> text1 :
#> [1] "b" "e" "f"
#> 
#> text2 :
#> [1] "B" "E" "F"
#> 
tokens_select(toks, c("b", "e", "f"), selection = "keep", padding = TRUE)
#> Tokens consisting of 2 documents.
#> text1 :
#>  [1] ""  "b" ""  ""  "e" "f" ""  ""  ""  ""  ""  "" 
#> [ ... and 14 more ]
#> 
#> text2 :
#>  [1] ""  "B" ""  ""  "E" "F" ""  ""  ""  ""  ""  "" 
#> [ ... and 14 more ]
#> 
tokens_select(toks, c("b", "e", "f"), selection = "remove", padding = FALSE)
#> Tokens consisting of 2 documents.
#> text1 :
#>  [1] "a" "c" "d" "g" "h" "i" "j" "k" "l" "m" "n" "o"
#> [ ... and 11 more ]
#> 
#> text2 :
#>  [1] "A" "C" "D" "G" "H" "I" "J" "K" "L" "M" "N" "O"
#> [ ... and 11 more ]
#> 
tokens_select(toks, c("b", "e", "f"), selection = "remove", padding = TRUE)
#> Tokens consisting of 2 documents.
#> text1 :
#>  [1] "a" ""  "c" "d" ""  ""  "g" "h" "i" "j" "k" "l"
#> [ ... and 14 more ]
#> 
#> text2 :
#>  [1] "A" ""  "C" "D" ""  ""  "G" "H" "I" "J" "K" "L"
#> [ ... and 14 more ]
#> 

# how case_insensitive works
tokens_select(toks, c("b", "e", "f"), selection = "remove", case_insensitive = TRUE)
#> Tokens consisting of 2 documents.
#> text1 :
#>  [1] "a" "c" "d" "g" "h" "i" "j" "k" "l" "m" "n" "o"
#> [ ... and 11 more ]
#> 
#> text2 :
#>  [1] "A" "C" "D" "G" "H" "I" "J" "K" "L" "M" "N" "O"
#> [ ... and 11 more ]
#> 
tokens_select(toks, c("b", "e", "f"), selection = "remove", case_insensitive = FALSE)
#> Tokens consisting of 2 documents.
#> text1 :
#>  [1] "a" "c" "d" "g" "h" "i" "j" "k" "l" "m" "n" "o"
#> [ ... and 11 more ]
#> 
#> text2 :
#>  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L"
#> [ ... and 14 more ]
#> 

# use window
tokens_select(toks, c("b", "f"), selection = "keep", window = 1)
#> Tokens consisting of 2 documents.
#> text1 :
#> [1] "a" "b" "c" "e" "f" "g"
#> 
#> text2 :
#> [1] "A" "B" "C" "E" "F" "G"
#> 
tokens_select(toks, c("b", "f"), selection = "remove", window = 1)
#> Tokens consisting of 2 documents.
#> text1 :
#>  [1] "d" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r"
#> [ ... and 8 more ]
#> 
#> text2 :
#>  [1] "D" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R"
#> [ ... and 8 more ]
#> 
tokens_remove(toks, c("b", "f"), window = c(0, 1))
#> Tokens consisting of 2 documents.
#> text1 :
#>  [1] "a" "d" "e" "h" "i" "j" "k" "l" "m" "n" "o" "p"
#> [ ... and 10 more ]
#> 
#> text2 :
#>  [1] "A" "D" "E" "H" "I" "J" "K" "L" "M" "N" "O" "P"
#> [ ... and 10 more ]
#> 
tokens_select(toks, pattern = c("e", "g"), window = c(1, 2))
#> Tokens consisting of 2 documents.
#> text1 :
#> [1] "d" "e" "f" "g" "h" "i"
#> 
#> text2 :
#> [1] "D" "E" "F" "G" "H" "I"
#> 

# tokens_remove example: remove stopwords
txt <- c(wash1 <- "Fellow citizens, I am again called upon by the voice of my
                   country to execute the functions of its Chief Magistrate.",
         wash2 <- "When the occasion proper for it shall arrive, I shall
                   endeavor to express the high sense I entertain of this
                   distinguished honor.")
tokens_remove(tokens(txt, remove_punct = TRUE), stopwords("english"))
#> Tokens consisting of 2 documents.
#> text1 :
#>  [1] "Fellow"     "citizens"   "called"     "upon"       "voice"     
#>  [6] "country"    "execute"    "functions"  "Chief"      "Magistrate"
#> 
#> text2 :
#>  [1] "occasion"      "proper"        "shall"         "arrive"       
#>  [5] "shall"         "endeavor"      "express"       "high"         
#>  [9] "sense"         "entertain"     "distinguished" "honor"        
#> 

# token_keep example: keep two-letter words
tokens_keep(tokens(txt, remove_punct = TRUE), "??")
#> Tokens consisting of 2 documents.
#> text1 :
#> [1] "am" "by" "of" "my" "to" "of"
#> 
#> text2 :
#> [1] "it" "to" "of"
#> 
```
