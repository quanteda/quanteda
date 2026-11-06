# Apply a dictionary to a dfm

Apply a dictionary to a dfm by looking up all dfm features for matches
in a a set of [dictionary](https://quanteda.io/reference/dictionary.md)
values, and replace those features with a count of the dictionary's
keys. If `exclusive = FALSE` then the behaviour is to apply a
"thesaurus", where each value match is replaced by the dictionary key,
converted to capitals if `capkeys = TRUE` (so that the replacements are
easily distinguished from features that were terms found originally in
the document).

## Usage

``` r
dfm_lookup(
  x,
  dictionary,
  levels = 1:5,
  exclusive = TRUE,
  valuetype = c("glob", "regex", "fixed"),
  case_insensitive = TRUE,
  capkeys = !exclusive,
  nomatch = NULL,
  verbose = quanteda_options("verbose")
)
```

## Arguments

- x:

  the dfm to which the dictionary will be applied

- dictionary:

  a [dictionary](https://quanteda.io/reference/dictionary.md)-class
  object

- levels:

  levels of entries in a hierarchical dictionary that will be applied

- exclusive:

  if `TRUE`, remove all features not in dictionary, otherwise, replace
  values in dictionary with keys while leaving other features unaffected

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
  from other features

- nomatch:

  an optional character naming a new feature that will contain the
  counts of features of `x` not matched to a dictionary key. If `NULL`
  (default), do not tabulate unmatched features.

- verbose:

  print status messages if `TRUE`

## Note

If using `dfm_lookup` with dictionaries containing multi-word values,
matches will only occur if the features themselves are multi-word or
formed from n-grams. A better way to match dictionary values that
include multi-word patterns is to apply
[`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md) to
the tokens, and then construct the dfm.

## See also

dfm_replace

## Examples

``` r
dict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
                        opposition = c("Opposition", "reject", "notincorpus"),
                        taxglob = "tax*",
                        taxregex = "tax.+$",
                        country = c("United_States", "Sweden")))
dfmat <- dfm(tokens(c("My Christmas was ruined by your opposition tax plan.",
                      "Does the United_States or Sweden have more progressive taxation?")))
dfmat
#> Document-feature matrix of: 2 documents, 20 features (50.00% sparse) and 0
#> docvars.
#>        features
#> docs    my christmas was ruined by your opposition tax plan .
#>   text1  1         1   1      1  1    1          1   1    1 1
#>   text2  0         0   0      0  0    0          0   0    0 0
#> [ reached max_nfeat ... 10 more features ]

# glob format
dfm_lookup(dfmat, dict, valuetype = "glob")
#> Document-feature matrix of: 2 documents, 5 features (50.00% sparse) and 0
#> docvars.
#>        features
#> docs    christmas opposition taxglob taxregex country
#>   text1         1          1       1        0       0
#>   text2         0          0       1        0       2
dfm_lookup(dfmat, dict, valuetype = "glob", case_insensitive = FALSE)
#> Document-feature matrix of: 2 documents, 5 features (50.00% sparse) and 0
#> docvars.
#>        features
#> docs    christmas opposition taxglob taxregex country
#>   text1         1          1       1        0       0
#>   text2         0          0       1        0       2

# regex v. glob format: note that "united_states" is a regex match for "tax*"
dfm_lookup(dfmat, dict, valuetype = "glob")
#> Document-feature matrix of: 2 documents, 5 features (50.00% sparse) and 0
#> docvars.
#>        features
#> docs    christmas opposition taxglob taxregex country
#>   text1         1          1       1        0       0
#>   text2         0          0       1        0       2
dfm_lookup(dfmat, dict, valuetype = "regex", case_insensitive = TRUE)
#> Document-feature matrix of: 2 documents, 5 features (40.00% sparse) and 0
#> docvars.
#>        features
#> docs    christmas opposition taxglob taxregex country
#>   text1         1          1       1        0       0
#>   text2         0          0       2        1       2

# fixed format: no pattern matching
dfm_lookup(dfmat, dict, valuetype = "fixed")
#> Document-feature matrix of: 2 documents, 5 features (70.00% sparse) and 0
#> docvars.
#>        features
#> docs    christmas opposition taxglob taxregex country
#>   text1         1          1       0        0       0
#>   text2         0          0       0        0       2
dfm_lookup(dfmat, dict, valuetype = "fixed", case_insensitive = FALSE)
#> Document-feature matrix of: 2 documents, 5 features (70.00% sparse) and 0
#> docvars.
#>        features
#> docs    christmas opposition taxglob taxregex country
#>   text1         1          1       0        0       0
#>   text2         0          0       0        0       2

# show unmatched tokens
dfm_lookup(dfmat, dict, nomatch = "_UNMATCHED")
#> Document-feature matrix of: 2 documents, 6 features (41.67% sparse) and 0
#> docvars.
#>        features
#> docs    christmas opposition taxglob taxregex country _UNMATCHED
#>   text1         1          1       1        0       0          7
#>   text2         0          0       1        0       2          7
```
