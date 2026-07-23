# Compute the sparsity of a document-feature matrix

Return the proportion of sparseness of a document-feature matrix, equal
to the proportion of cells that have zero counts.

## Usage

``` r
sparsity(x)
```

## Arguments

- x:

  the document-feature matrix

## Examples

``` r
dfmat <- dfm(tokens(data_corpus_inaugural))
sparsity(dfmat)
#> [1] 0.9193654
sparsity(dfm_trim(dfmat, min_termfreq = 5))
#> [1] 0.7871639
```
