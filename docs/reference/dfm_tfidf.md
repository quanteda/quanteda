# Weight a dfm by *tf-idf*

Weight a dfm by term frequency-inverse document frequency (*tf-idf*),
with full control over options. Uses fully sparse methods for
efficiency.

## Usage

``` r
dfm_tfidf(
  x,
  scheme_tf = "count",
  scheme_df = "inverse",
  base = 10,
  force = FALSE,
  ...
)
```

## Arguments

- x:

  object for which idf or tf-idf will be computed (a document-feature
  matrix)

- scheme_tf:

  scheme for
  [`dfm_weight()`](https://quanteda.io/reference/dfm_weight.md);
  defaults to `"count"`

- scheme_df:

  scheme for [`docfreq()`](https://quanteda.io/reference/docfreq.md);
  defaults to `"inverse"`.

- base:

  the base for the logarithms in the
  [`dfm_weight()`](https://quanteda.io/reference/dfm_weight.md) and
  [`docfreq()`](https://quanteda.io/reference/docfreq.md) calls; default
  is 10

- force:

  logical; if `TRUE`, apply weighting scheme even if the dfm has been
  weighted before. This can result in invalid weights, such as as
  weighting by `"prop"` after applying `"logcount"`, or after having
  grouped a dfm using
  [`dfm_group()`](https://quanteda.io/reference/dfm_group.md).

- ...:

  additional arguments passed to
  [`docfreq`](https://quanteda.io/reference/docfreq.md).

## Details

`dfm_tfidf` computes term frequency-inverse document frequency
weighting. The default is to use counts instead of normalized term
frequency (the relative term frequency within document), but this can be
overridden using `scheme_tf = "prop"`.

## References

Manning, C. D., Raghavan, P., & Schütze, H. (2008). *Introduction to
Information Retrieval*. Cambridge: Cambridge University Press.
<https://nlp.stanford.edu/IR-book/pdf/irbookonlinereading.pdf>

## Examples

``` r
dfmat1 <- as.dfm(data_dfm_lbgexample)
head(dfmat1[, 5:10])
#> Document-feature matrix of: 6 documents, 6 features (61.11% sparse) and 0
#> docvars.
#>     features
#> docs  E  F   G   H   I   J
#>   R1 45 78 115 146 158 146
#>   R2  0  2   3  10  22  45
#>   R3  0  0   0   0   0   0
#>   R4  0  0   0   0   0   0
#>   R5  0  0   0   0   0   0
#>   V1  0  0   0   2   3  10
head(dfm_tfidf(dfmat1)[, 5:10])
#> Document-feature matrix of: 6 documents, 6 features (61.11% sparse) and 0
#> docvars.
#>     features
#> docs        E          F         G        H        I        J
#>   R1 35.01681 37.2154579 54.868944 43.95038 47.56274 43.95038
#>   R2  0        0.9542425  1.431364  3.01030  6.62266 13.54635
#>   R3  0        0          0         0        0        0      
#>   R4  0        0          0         0        0        0      
#>   R5  0        0          0         0        0        0      
#>   V1  0        0          0         0.60206  0.90309  3.01030
docfreq(dfmat1)[5:15]
#> E F G H I J K L M N O 
#> 1 2 2 3 3 3 4 4 4 4 4 
head(dfm_weight(dfmat1)[, 5:10])
#> Document-feature matrix of: 6 documents, 6 features (61.11% sparse) and 0
#> docvars.
#>     features
#> docs  E  F   G   H   I   J
#>   R1 45 78 115 146 158 146
#>   R2  0  2   3  10  22  45
#>   R3  0  0   0   0   0   0
#>   R4  0  0   0   0   0   0
#>   R5  0  0   0   0   0   0
#>   V1  0  0   0   2   3  10

# replication of worked example from
# https://en.wikipedia.org/wiki/Tf-idf#Example_of_tf.E2.80.93idf
dfmat2 <-
    matrix(c(1,1,2,1,0,0, 1,1,0,0,2,3),
           byrow = TRUE, nrow = 2,
           dimnames = list(docs = c("document1", "document2"),
                           features = c("this", "is", "a", "sample",
                                        "another", "example"))) |>
    as.dfm()
dfmat2
#> Document-feature matrix of: 2 documents, 6 features (33.33% sparse) and 0
#> docvars.
#>            features
#> docs        this is a sample another example
#>   document1    1  1 2      1       0       0
#>   document2    1  1 0      0       2       3
docfreq(dfmat2)
#>    this      is       a  sample another example 
#>       2       2       1       1       1       1 
dfm_tfidf(dfmat2, scheme_tf = "prop") |> round(digits = 2)
#> Document-feature matrix of: 2 documents, 6 features (33.33% sparse) and 0
#> docvars.
#>            features
#> docs        this is    a sample another example
#>   document1    0  0 0.12   0.06    0       0   
#>   document2    0  0 0      0       0.09    0.13

if (FALSE) { # \dontrun{
# comparison with tm
if (requireNamespace("tm")) {
    convert(dfmat2, to = "tm") |> tm::weightTfIdf() |> as.matrix()
    # same as:
    dfm_tfidf(dfmat2, base = 2, scheme_tf = "prop")
}
} # }
```
