# Bootstrap a dfm

Create an array of resampled dfms.

## Usage

``` r
bootstrap_dfm(x, n = 10, ..., verbose = quanteda_options("verbose"))
```

## Arguments

- x:

  a [dfm](https://quanteda.io/reference/dfm.md) object

- n:

  number of resamples

- ...:

  additional arguments passed to
  [`dfm()`](https://quanteda.io/reference/dfm.md)

- verbose:

  if `TRUE` print status messages

## Value

A named list of [dfm](https://quanteda.io/reference/dfm.md) objects,
where the first, `dfm_0`, is the dfm from the original texts, and
subsequent elements are the sentence-resampled dfms.

## Details

Function produces multiple, resampled
[dfm](https://quanteda.io/reference/dfm.md) objects, based on resampling
sentences (with replacement) from each document, recombining these into
new "documents" and computing a dfm for each. Resampling of sentences is
done strictly within document, so that every resampled document will
contain at least some of its original tokens.

## Author

Kenneth Benoit

## Examples

``` r
set.seed(10)
txt <- c(textone = "This is a sentence.  Another sentence.  Yet another.",
         texttwo = "Premiere phrase.  Deuxieme phrase.")
dfmat <- corpus_reshape(corpus(txt), to = "sentences") |>
    tokens() |>
    dfm()
bootstrap_dfm(dfmat, n = 3)
#> 
#> $dfm_0
#> Document-feature matrix of: 2 documents, 10 features (45.00% sparse) and 0
#> docvars.
#>          features
#> docs      this is a sentence . another yet premiere phrase deuxieme
#>   textone    1  1 1        2 3       2   1        0      0        0
#>   texttwo    0  0 0        0 2       0   0        1      2        1
#> 
#> $dfm_1
#> Document-feature matrix of: 2 documents, 10 features (50.00% sparse) and 0
#> docvars.
#>          features
#> docs      this is a sentence . another yet premiere phrase deuxieme
#>   textone    1  1 1        2 3       2   1        0      0        0
#>   texttwo    0  0 0        0 2       0   0        0      2        2
#> 
#> $dfm_2
#> Document-feature matrix of: 2 documents, 10 features (65.00% sparse) and 0
#> docvars.
#>          features
#> docs      this is a sentence . another yet premiere phrase deuxieme
#>   textone    0  0 0        1 3       3   2        0      0        0
#>   texttwo    0  0 0        0 2       0   0        2      2        0
#> 
#> $dfm_3
#> Document-feature matrix of: 2 documents, 10 features (60.00% sparse) and 0
#> docvars.
#>          features
#> docs      this is a sentence . another yet premiere phrase deuxieme
#>   textone    0  0 0        1 3       3   2        0      0        0
#>   texttwo    0  0 0        0 2       0   0        1      2        1
#> 
#> attr(,"class")
#> [1] "dfm_bootstrap"
```
