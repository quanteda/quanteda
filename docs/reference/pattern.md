# Pattern for feature, token and keyword matching

Pattern(s) for use in matching features, tokens, and keywords through a
[valuetype](https://quanteda.io/reference/valuetype.md) pattern.

## Arguments

- pattern:

  a character vector, list of character vectors,
  [dictionary](https://quanteda.io/reference/dictionary.md), or
  collocations object. See pattern for details.

## Details

The `pattern` argument is a vector of patterns, including sequences, to
match in a target object, whose match type is specified by
[valuetype](https://quanteda.io/reference/valuetype.md). Note that an
empty pattern (`""`) will match "padding" in a
[tokens](https://quanteda.io/reference/tokens.md) object.

- `character`:

  A character vector of token patterns to be selected or removed.
  Whitespace is not privileged, so that in a character vector, white
  space is interpreted literally. If you wish to consider
  whitespace-separated elements as sequences of tokens, wrap the
  argument in [`phrase()`](https://quanteda.io/reference/phrase.md).

- `list of character objects`:

  If the list elements are character vectors of length 1, then this is
  equivalent to a vector of characters. If a list element contains a
  vector of characters longer than length 1, then for matching will
  consider these as sequences of matches, equivalent to wrapping the
  argument in [`phrase()`](https://quanteda.io/reference/phrase.md),
  except for matching to [dfm](https://quanteda.io/reference/dfm.md)
  features where this does not apply.

- `dictionary`:

  Values in [dictionary](https://quanteda.io/reference/dictionary.md)
  are used as patterns, for literal matches. Multi-word values are
  automatically converted into phrases, so performing selection or
  compounding using a dictionary is the same as wrapping the dictionary
  in [`phrase()`](https://quanteda.io/reference/phrase.md).

- `collocations`:

  Collocations objects created from
  [`quanteda.textstats::textstat_collocations()`](https://quanteda.io/reference/textstat_collocations.html),
  which are treated as phrases automatically.

## See also

[valuetype](https://quanteda.io/reference/valuetype.md),
[case_insensitive](https://quanteda.io/reference/valuetype.md)

## Examples

``` r
# these are interpreted literally
(patt1 <- c("president", "white house", "house of representatives"))
#> [1] "president"                "white house"             
#> [3] "house of representatives"
# as multi-word sequences
phrase(patt1)
#> [[1]]
#> [1] "president"
#> 
#> [[2]]
#> [1] "white" "house"
#> 
#> [[3]]
#> [1] "house"           "of"              "representatives"
#> 

# three single-word patterns
(patt2 <- c("president", "white_house", "house_of_representatives"))
#> [1] "president"                "white_house"             
#> [3] "house_of_representatives"
phrase(patt2)
#> [[1]]
#> [1] "president"
#> 
#> [[2]]
#> [1] "white_house"
#> 
#> [[3]]
#> [1] "house_of_representatives"
#> 

# this is equivalent to phrase(patt1)
(patt3 <- list(c("president"), c("white", "house"),
               c("house", "of", "representatives")))
#> [[1]]
#> [1] "president"
#> 
#> [[2]]
#> [1] "white" "house"
#> 
#> [[3]]
#> [1] "house"           "of"              "representatives"
#> 

# glob expression can be used
phrase(patt4 <- c("president?", "white house", "house * representatives"))
#> [[1]]
#> [1] "president?"
#> 
#> [[2]]
#> [1] "white" "house"
#> 
#> [[3]]
#> [1] "house"           "*"               "representatives"
#> 

# this is equivalent to phrase(patt4)
(patt5 <- list(c("president?"), c("white", "house"), c("house", "*", "representatives")))
#> [[1]]
#> [1] "president?"
#> 
#> [[2]]
#> [1] "white" "house"
#> 
#> [[3]]
#> [1] "house"           "*"               "representatives"
#> 

# dictionary with multi-word matches
(dict1 <- dictionary(list(us = c("president", "white house", "house of representatives"))))
#> Dictionary object with 1 key entry.
#> - [us]:
#>   - president, white house, house of representatives
phrase(dict1)
#> [[1]]
#> [1] "president"
#> 
#> [[2]]
#> [1] "white" "house"
#> 
#> [[3]]
#> [1] "house"           "of"              "representatives"
#> 
```
