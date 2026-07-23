# Count the number of documents or features

Get the number of documents or features in an object.

## Usage

``` r
ndoc(x)

nfeat(x)
```

## Arguments

- x:

  a quanteda object: a
  [corpus](https://quanteda.io/reference/corpus.md),
  [dfm](https://quanteda.io/reference/dfm.md),
  [tokens](https://quanteda.io/reference/tokens.md), or
  [tokens_xptr](https://quanteda.io/reference/tokens_xptr.md) object, or
  a readtext object from the readtext package

## Value

`ndoc()` returns an integer count of the number of documents in an
object whose texts are organized as "documents" (a
[corpus](https://quanteda.io/reference/corpus.md),
[dfm](https://quanteda.io/reference/dfm.md), or
[tokens](https://quanteda.io/reference/tokens.md)/[tokens_xptr](https://quanteda.io/reference/tokens_xptr.md)
object.

`nfeat()` returns an integer count of the number of features. It is an
alias for [`ntype()`](https://quanteda.io/reference/ntoken.md) for a
dfm. This function is only defined for
[dfm](https://quanteda.io/reference/dfm.md) objects because only these
have "features".

## See also

[`ntoken()`](https://quanteda.io/reference/ntoken.md),
[`ntype()`](https://quanteda.io/reference/ntoken.md)

## Examples

``` r
# number of documents
ndoc(data_corpus_inaugural)
#> [1] 60
ndoc(corpus_subset(data_corpus_inaugural, Year > 1980))
#> [1] 12
ndoc(tokens(data_corpus_inaugural))
#> [1] 60
ndoc(dfm(tokens(corpus_subset(data_corpus_inaugural, Year > 1980))))
#> [1] 12

# number of features
toks1 <- tokens(corpus_subset(data_corpus_inaugural, Year > 1980), remove_punct = FALSE)
toks2 <- tokens(corpus_subset(data_corpus_inaugural, Year > 1980), remove_punct = TRUE)
nfeat(dfm(toks1))
#> [1] 3756
nfeat(dfm(toks2))
#> [1] 3740
```
