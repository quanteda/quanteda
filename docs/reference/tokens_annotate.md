# Annotate a tokens object using a dictionary

Insert dictionary keys as tags in a tokens object where the dictionary
patterns are found.

## Usage

``` r
tokens_annotate(
  x,
  dictionary,
  levels = 1:5,
  valuetype = c("glob", "regex", "fixed"),
  case_insensitive = TRUE,
  marker = "#",
  capkeys = TRUE,
  nested_scope = c("key", "dictionary"),
  apply_if = NULL,
  verbose = quanteda_options("verbose")
)
```

## Arguments

- x:

  the [tokens](https://quanteda.io/reference/tokens.md) object to which
  the dictionary will be applied

- dictionary:

  the [dictionary](https://quanteda.io/reference/dictionary.md)-class
  object that will be applied to `x`

- levels:

  integers specifying the levels of entries in a hierarchical dictionary
  that will be applied. The top level is 1, and subsequent levels
  describe lower nesting levels. Values may be combined, even if these
  levels are not contiguous, e.g. `levels = c(1:3)` will collapse the
  second level into the first, but record the third level (if present)
  collapsed below the first (see examples).

- valuetype:

  the type of pattern matching: `"glob"` for "glob"-style wildcard
  expressions; `"regex"` for regular expressions; or `"fixed"` for exact
  matching. See [valuetype](https://quanteda.io/reference/valuetype.md)
  for details.

- case_insensitive:

  logical; if `TRUE`, ignore case when matching a `pattern` or
  [dictionary](https://quanteda.io/reference/dictionary.md) values

- marker:

  characters that are added before and after the dictionary keys to
  create tags.

- capkeys:

  if `TRUE`, convert dictionary keys to uppercase to distinguish them
  from unmatched tokens.

- nested_scope:

  how to treat matches from different dictionary keys that are nested.
  When one value is nested within another, such as "a b" being nested
  within "a b c", then
  [`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md)
  will match the longer. When `nested_scope = "key"`, this longer-match
  priority is applied only within the key, while `"dictionary"` applies
  it across keys, matching only the key with the longer pattern, not the
  matches nested within that longer pattern from other keys. See
  Details.

- apply_if:

  logical vector of length `ndoc(x)`; documents are modified only when
  corresponding values are `TRUE`, others are left unchanged.

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## See also

tokens_lookup

## Examples

``` r
txt <- c(d1 = "The United States has the Atlantic Ocean and the Pacific Ocean.",
         d2 = "Britain and Ireland have the Irish Sea and the English Channel.")
toks <- tokens(txt)
dict <- dictionary(list(US = list(Countries = c("States"),
                                  oceans = c("Atlantic", "Pacific")),
                        Europe = list(Countries = c("Britain", "Ireland"),
                                      oceans = list(west = "Irish Sea",
                                                    east = "English Channel"))))
tokens_annotate(toks, dict)
#> Tokens consisting of 2 documents.
#> d1 :
#>  [1] "The"            "United"         "States"         "#US.COUNTRIES#"
#>  [5] "has"            "the"            "Atlantic"       "#US.OCEANS#"   
#>  [9] "Ocean"          "and"            "the"            "Pacific"       
#> [ ... and 3 more ]
#> 
#> d2 :
#>  [1] "Britain"              "#EUROPE.COUNTRIES#"   "and"                 
#>  [4] "Ireland"              "#EUROPE.COUNTRIES#"   "have"                
#>  [7] "the"                  "Irish"                "Sea"                 
#> [10] "#EUROPE.OCEANS.WEST#" "and"                  "the"                 
#> [ ... and 4 more ]
#> 
```
