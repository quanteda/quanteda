# Apply a dictionary to a tokens object

Convert tokens into equivalence classes defined by values of a
dictionary object.

## Usage

``` r
tokens_lookup(
  x,
  dictionary,
  levels = 1:5,
  valuetype = c("glob", "regex", "fixed"),
  case_insensitive = TRUE,
  capkeys = !exclusive,
  exclusive = TRUE,
  nomatch = NULL,
  append_key = FALSE,
  separator = "/",
  concatenator = concat(x),
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

- capkeys:

  if `TRUE`, convert dictionary keys to uppercase to distinguish them
  from unmatched tokens.

- exclusive:

  if `TRUE`, remove all features not in dictionary, otherwise, replace
  values in dictionary with keys while leaving other features
  unaffected.

- nomatch:

  an optional character naming a new key for tokens that do not matched
  to a dictionary values If `NULL` (default), do not record unmatched
  tokens.

- append_key:

  if `TRUE`, annotate matched tokens with keys.

- separator:

  a character to separate tokens and keys when `append_key = TRUE`.

- concatenator:

  the concatenation character that will connect the words making up the
  multi-word sequences.

- nested_scope:

  how to treat matches from different dictionary keys that are nested.
  When one value is nested within another, such as "a b" being nested
  within "a b c", then `tokens_lookup()` will match the longer. When
  `nested_scope = "key"`, this longer-match priority is applied only
  within the key, while `"dictionary"` applies it across keys, matching
  only the key with the longer pattern, not the matches nested within
  that longer pattern from other keys. See Details.

- apply_if:

  logical vector of length `ndoc(x)`; documents are modified only when
  corresponding values are `TRUE`, others are left unchanged.

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## Details

Dictionary values may consist of sequences, and there are different
methods of counting key matches based on values that are nested or that
overlap.

When two different keys in a dictionary are nested matches of one
another, the `nested_scope` options provide the choice of matching each
key's values independently (the `"key"`) option, or just counting the
longest match (the `"dictionary"` option). Values that are nested
*within* the same key are always counted as a single match. See the last
example below comparing the *New York* and *New York Times* for these
two different behaviours.

*Overlapping values*, such as `"a b"` and `"b a"` are currently always
considered as separate matches if they are in different keys, or as one
match if the overlap is within the same key.

Note: `apply_if` This applies the dictionary lookup only to documents
that match the logical condition. When `exclusive = TRUE` (the default),
however, this means that empty documents will be returned for those not
meeting the condition, since no lookup will be applied and hence no
tokens replaced by matching keys.

## See also

tokens_replace

## Examples

``` r
toks1 <- tokens(data_corpus_inaugural)
dict1 <- dictionary(list(country = "united states",
                   law=c("law*", "constitution"),
                   freedom=c("free*", "libert*")))
dfm(tokens_lookup(toks1, dict1, valuetype = "glob", verbose = TRUE))
#> tokens_lookup() changed from 10,332 types (60 documents, 154,888 tokens) to 3 types (60 documents, 1,211 tokens)
#> Document-feature matrix of: 60 documents, 3 features (14.44% sparse) and 4
#> docvars.
#>                  features
#> docs              country law freedom
#>   1789-Washington       2   1       6
#>   1793-Washington       0   1       0
#>   1797-Adams            3  10       6
#>   1801-Jefferson        0   6      11
#>   1805-Jefferson        1  13       6
#>   1809-Madison          2   2       5
#> [ reached max_ndoc ... 54 more documents ]
dfm(tokens_lookup(toks1, dict1, valuetype = "glob", verbose = TRUE, nomatch = "NONE"))
#> tokens_lookup() changed from 10,332 types (60 documents, 154,888 tokens) to 4 types (60 documents, 154,723 tokens)
#> Document-feature matrix of: 60 documents, 4 features (10.83% sparse) and 4
#> docvars.
#>                  features
#> docs              country law freedom none
#>   1789-Washington       2   1       6 1526
#>   1793-Washington       0   1       0  146
#>   1797-Adams            3  10       6 2555
#>   1801-Jefferson        0   6      11 1906
#>   1805-Jefferson        1  13       6 2359
#>   1809-Madison          2   2       5 1250
#> [ reached max_ndoc ... 54 more documents ]

dict2 <- dictionary(list(country = "united states",
                       law = c("law", "constitution"),
                       freedom = c("freedom", "liberty")))
# dfm(applyDictionary(toks1, dict2, valuetype = "fixed"))
dfm(tokens_lookup(toks1, dict2, valuetype = "fixed"))
#> Document-feature matrix of: 60 documents, 3 features (20.56% sparse) and 4
#> docvars.
#>                  features
#> docs              country law freedom
#>   1789-Washington       2   1       1
#>   1793-Washington       0   1       0
#>   1797-Adams            3   8       3
#>   1801-Jefferson        0   6       7
#>   1805-Jefferson        1   9       5
#>   1809-Madison          2   2       3
#> [ reached max_ndoc ... 54 more documents ]

# hierarchical dictionary example
txt <- c(d1 = "The United States has the Atlantic Ocean and the Pacific Ocean.",
         d2 = "Britain and Ireland have the Irish Sea and the English Channel.")
toks2 <- tokens(txt)
dict3 <- dictionary(list(US = list(Countries = c("States"),
                                  oceans = c("Atlantic", "Pacific")),
                        Europe = list(Countries = c("Britain", "Ireland"),
                                      oceans = list(west = "Irish Sea",
                                                    east = "English Channel"))))
tokens_lookup(toks2, dict3, levels = 1)
#> Tokens consisting of 2 documents.
#> d1 :
#> [1] "US" "US" "US"
#> 
#> d2 :
#> [1] "Europe" "Europe" "Europe" "Europe"
#> 
tokens_lookup(toks2, dict3, levels = 2)
#> Tokens consisting of 2 documents.
#> d1 :
#> [1] "Countries" "oceans"    "oceans"   
#> 
#> d2 :
#> [1] "Countries" "Countries" "oceans"    "oceans"   
#> 
tokens_lookup(toks2, dict3, levels = 1:2)
#> Tokens consisting of 2 documents.
#> d1 :
#> [1] "US.Countries" "US.oceans"    "US.oceans"   
#> 
#> d2 :
#> [1] "Europe.Countries" "Europe.Countries" "Europe.oceans"    "Europe.oceans"   
#> 
tokens_lookup(toks2, dict3, levels = 3)
#> Tokens consisting of 2 documents.
#> d1 :
#> character(0)
#> 
#> d2 :
#> [1] "west" "east"
#> 
tokens_lookup(toks2, dict3, levels = c(1,3))
#> Tokens consisting of 2 documents.
#> d1 :
#> [1] "US" "US" "US"
#> 
#> d2 :
#> [1] "Europe"      "Europe"      "Europe.west" "Europe.east"
#> 
tokens_lookup(toks2, dict3, levels = c(2,3))
#> Tokens consisting of 2 documents.
#> d1 :
#> [1] "Countries" "oceans"    "oceans"   
#> 
#> d2 :
#> [1] "Countries"   "Countries"   "oceans.west" "oceans.east"
#> 

# show unmatched tokens
tokens_lookup(toks2, dict3, nomatch = "_UNMATCHED")
#> Tokens consisting of 2 documents.
#> d1 :
#>  [1] "_UNMATCHED"   "_UNMATCHED"   "US.Countries" "_UNMATCHED"   "_UNMATCHED"  
#>  [6] "US.oceans"    "_UNMATCHED"   "_UNMATCHED"   "_UNMATCHED"   "US.oceans"   
#> [11] "_UNMATCHED"   "_UNMATCHED"  
#> 
#> d2 :
#>  [1] "Europe.Countries"   "_UNMATCHED"         "Europe.Countries"  
#>  [4] "_UNMATCHED"         "_UNMATCHED"         "Europe.oceans.west"
#>  [7] "_UNMATCHED"         "_UNMATCHED"         "Europe.oceans.east"
#> [10] "_UNMATCHED"        
#> 

# nested matching differences
dict4 <- dictionary(list(paper = "New York Times", city = "New York"))
toks4 <- tokens("The New York Times is a New York paper.")
tokens_lookup(toks4, dict4, nested_scope = "key", exclusive = FALSE)
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "The"   "PAPER" "CITY"  "is"    "a"     "CITY"  "paper" "."    
#> 
tokens_lookup(toks4, dict4, nested_scope = "dictionary", exclusive = FALSE)
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "The"   "PAPER" "is"    "a"     "CITY"  "paper" "."    
#> 
```
