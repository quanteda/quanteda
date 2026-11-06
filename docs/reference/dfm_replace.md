# Replace features in dfm

Substitute features based on vectorized one-to-one matching for
lemmatization or user-defined stemming.

## Usage

``` r
dfm_replace(
  x,
  pattern,
  replacement,
  case_insensitive = TRUE,
  verbose = quanteda_options("verbose")
)
```

## Arguments

- x:

  [dfm](https://quanteda.io/reference/dfm.md) whose features will be
  replaced

- pattern:

  a character vector. See
  [pattern](https://quanteda.io/reference/pattern.md) for more details.

- replacement:

  if `pattern` is a character vector, then `replacement` must be
  character vector of equal length, for a 1:1 match.

- case_insensitive:

  logical; if `TRUE`, ignore case when matching a `pattern` or
  [dictionary](https://quanteda.io/reference/dictionary.md) values

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## Examples

``` r
dfmat1 <- dfm(tokens(data_corpus_inaugural))

# lemmatization
taxwords <- c("tax", "taxing", "taxed", "taxed", "taxation")
lemma <- rep("TAX", length(taxwords))
featnames(dfm_select(dfmat1, pattern = taxwords))
#> [1] "tax"      "taxation" "taxing"  
dfmat2 <- dfm_replace(dfmat1, pattern = taxwords, replacement = lemma)
featnames(dfm_select(dfmat2, pattern = taxwords))
#> [1] "TAX"

# stemming
feat <- featnames(dfmat1)
featstem <- char_wordstem(feat, "porter")
dfmat3 <- dfm_replace(dfmat1, pattern = feat, replacement = featstem, case_insensitive = FALSE)
identical(dfmat3, dfm_wordstem(dfmat1, "porter"))
#> [1] TRUE
```
