# Select features from a dfm or fcm

This function selects or removes features from a
[dfm](https://quanteda.io/reference/dfm.md) or
[fcm](https://quanteda.io/reference/fcm.md), based on feature name
matches with `pattern`. The most common usages are to eliminate features
from a dfm already constructed, such as stopwords, or to select only
terms of interest from a dictionary.

## Usage

``` r
dfm_select(
  x,
  pattern = NULL,
  selection = c("keep", "remove"),
  valuetype = c("glob", "regex", "fixed"),
  case_insensitive = TRUE,
  min_nchar = NULL,
  max_nchar = NULL,
  padding = FALSE,
  verbose = quanteda_options("verbose")
)

dfm_remove(x, ...)

dfm_keep(x, ...)

fcm_select(
  x,
  pattern = NULL,
  selection = c("keep", "remove"),
  valuetype = c("glob", "regex", "fixed"),
  case_insensitive = TRUE,
  verbose = quanteda_options("verbose"),
  ...
)

fcm_remove(x, ...)

fcm_keep(x, ...)
```

## Arguments

- x:

  the [dfm](https://quanteda.io/reference/dfm.md) or
  [fcm](https://quanteda.io/reference/fcm.md) object whose features will
  be selected

- pattern:

  a character vector, list of character vectors,
  [dictionary](https://quanteda.io/reference/dictionary.md), or
  collocations object. See
  [pattern](https://quanteda.io/reference/pattern.md) for details.

- selection:

  whether to `keep` or `remove` the features

- valuetype:

  the type of pattern matching: `"glob"` for "glob"-style wildcard
  expressions; `"regex"` for regular expressions; or `"fixed"` for exact
  matching. See [valuetype](https://quanteda.io/reference/valuetype.md)
  for details.

- case_insensitive:

  logical; if `TRUE`, ignore case when matching a `pattern` or
  [dictionary](https://quanteda.io/reference/dictionary.md) values

- min_nchar, max_nchar:

  optional numerics specifying the minimum and maximum length in
  characters for tokens to be removed or kept; defaults are `NULL` for
  no limits. These are applied after (and hence, in addition to) any
  selection based on pattern matches.

- padding:

  if `TRUE`, record the number of removed tokens in the first column.

- verbose:

  if `TRUE`, print message about how many pattern were removed

- ...:

  used only for passing arguments from `dfm_remove` or `dfm_keep` to
  `dfm_select`. Cannot include `selection`.

## Value

A [dfm](https://quanteda.io/reference/dfm.md) or
[fcm](https://quanteda.io/reference/fcm.md) object, after the feature
selection has been applied.

For compatibility with earlier versions, when `pattern` is a
[dfm](https://quanteda.io/reference/dfm.md) object and
`selection = "keep"`, then this will be equivalent to calling
[`dfm_match()`](https://quanteda.io/reference/dfm_match.md). In this
case, the following settings are always used:
`case_insensitive = FALSE`, and `valuetype = "fixed"`. This
functionality is deprecated, however, and you should use
[`dfm_match()`](https://quanteda.io/reference/dfm_match.md) instead.

## Details

`dfm_remove` and `fcm_remove` are simply a convenience wrappers to
calling `dfm_select` and `fcm_select` with `selection = "remove"`.

`dfm_keep` and `fcm_keep` are simply a convenience wrappers to calling
`dfm_select` and `fcm_select` with `selection = "keep"`.

## Note

This function selects features based on their labels. To select features
based on the values of the document-feature matrix, use
[`dfm_trim()`](https://quanteda.io/reference/dfm_trim.md).

## See also

[`dfm_match()`](https://quanteda.io/reference/dfm_match.md)

## Examples

``` r
dfmat <- tokens(c("My Christmas was ruined by your opposition tax plan.",
               "Does the United_States or Sweden have more progressive taxation?")) |>
    dfm(tolower = FALSE)
dict <- dictionary(list(countries = c("United_States", "Sweden", "France"),
                        wordsEndingInY = c("by", "my"),
                        notintext = "blahblah"))
dfm_select(dfmat, pattern = dict)
#> Document-feature matrix of: 2 documents, 4 features (50.00% sparse) and 0
#> docvars.
#>        features
#> docs    My by United_States Sweden
#>   text1  1  1             0      0
#>   text2  0  0             1      1
dfm_select(dfmat, pattern = dict, case_insensitive = FALSE)
#> Document-feature matrix of: 2 documents, 1 feature (50.00% sparse) and 0
#> docvars.
#>        features
#> docs    by
#>   text1  1
#>   text2  0
dfm_select(dfmat, pattern = c("s$", ".y"), selection = "keep", valuetype = "regex")
#> Document-feature matrix of: 2 documents, 6 features (50.00% sparse) and 0
#> docvars.
#>        features
#> docs    My Christmas was by Does United_States
#>   text1  1         1   1  1    0             0
#>   text2  0         0   0  0    1             1
dfm_select(dfmat, pattern = c("s$", ".y"), selection = "remove", valuetype = "regex")
#> Document-feature matrix of: 2 documents, 14 features (50.00% sparse) and 0
#> docvars.
#>        features
#> docs    ruined your opposition tax plan . the or Sweden have
#>   text1      1    1          1   1    1 1   0  0      0    0
#>   text2      0    0          0   0    0 0   1  1      1    1
#> [ reached max_nfeat ... 4 more features ]
dfm_select(dfmat, pattern = stopwords("english"), selection = "keep", valuetype = "fixed")
#> Document-feature matrix of: 2 documents, 9 features (50.00% sparse) and 0
#> docvars.
#>        features
#> docs    My was by your Does the or have more
#>   text1  1   1  1    1    0   0  0    0    0
#>   text2  0   0  0    0    1   1  1    1    1
dfm_select(dfmat, pattern = stopwords("english"), selection = "remove", valuetype = "fixed")
#> Document-feature matrix of: 2 documents, 11 features (50.00% sparse) and 0
#> docvars.
#>        features
#> docs    Christmas ruined opposition tax plan . United_States Sweden progressive
#>   text1         1      1          1   1    1 1             0      0           0
#>   text2         0      0          0   0    0 0             1      1           1
#>        features
#> docs    taxation
#>   text1        0
#>   text2        1
#> [ reached max_nfeat ... 1 more feature ]

# select based on character length
dfm_select(dfmat, min_nchar = 5)
#> Document-feature matrix of: 2 documents, 7 features (50.00% sparse) and 0
#> docvars.
#>        features
#> docs    Christmas ruined opposition United_States Sweden progressive taxation
#>   text1         1      1          1             0      0           0        0
#>   text2         0      0          0             1      1           1        1

dfmat <- dfm(tokens(c("This is a document with lots of stopwords.",
                      "No if, and, or but about it: lots of stopwords.")))
dfmat
#> Document-feature matrix of: 2 documents, 18 features (38.89% sparse) and 0
#> docvars.
#>        features
#> docs    this is a document with lots of stopwords . no
#>   text1    1  1 1        1    1    1  1         1 1  0
#>   text2    0  0 0        0    0    1  1         1 1  1
#> [ reached max_nfeat ... 8 more features ]
dfm_remove(dfmat, stopwords("english"))
#> Document-feature matrix of: 2 documents, 6 features (25.00% sparse) and 0
#> docvars.
#>        features
#> docs    document lots stopwords . , :
#>   text1        1    1         1 1 0 0
#>   text2        0    1         1 1 2 1
toks <- tokens(c("this contains lots of stopwords",
                 "no if, and, or but about it: lots"),
               remove_punct = TRUE)
fcmat <- fcm(toks)
fcmat
#> Feature co-occurrence matrix of: 12 by 12 features.
#>            features
#> features    this contains lots of stopwords no if and or but
#>   this         0        1    1  1         1  0  0   0  0   0
#>   contains     0        0    1  1         1  0  0   0  0   0
#>   lots         0        0    0  1         1  1  1   1  1   1
#>   of           0        0    0  0         1  0  0   0  0   0
#>   stopwords    0        0    0  0         0  0  0   0  0   0
#>   no           0        0    0  0         0  0  1   1  1   1
#>   if           0        0    0  0         0  0  0   1  1   1
#>   and          0        0    0  0         0  0  0   0  1   1
#>   or           0        0    0  0         0  0  0   0  0   1
#>   but          0        0    0  0         0  0  0   0  0   0
#> [ reached max_nfeat ... 2 more features, reached max_nfeat ... 2 more features ]
fcm_remove(fcmat, stopwords("english"))
#> Feature co-occurrence matrix of: 3 by 3 features.
#>            features
#> features    contains lots stopwords
#>   contains         0    1         1
#>   lots             0    0         1
#>   stopwords        0    0         0
```
