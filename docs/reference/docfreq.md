# Compute the (weighted) document frequency of a feature

For a [dfm](https://quanteda.io/reference/dfm.md) object, returns a
(weighted) document frequency for each term. The default is a simple
count of the number of documents in which a feature occurs more than a
given frequency threshold. (The default threshold is zero, meaning that
any feature occurring at least once in a document will be counted.)

## Usage

``` r
docfreq(
  x,
  scheme = c("count", "inverse", "inversemax", "inverseprob", "unary"),
  base = 10,
  smoothing = 0,
  k = 0,
  threshold = 0
)
```

## Arguments

- x:

  a [dfm](https://quanteda.io/reference/dfm.md)

- scheme:

  type of document frequency weighting, computed as follows, where \\N\\
  is defined as the number of documents in the dfm and \\s\\ is the
  smoothing constant:

  `count`

  : \\df_j\\, the number of documents for which \\n\_{ij} \> threshold\\

  `inverse`

  : \$\$\textrm{log}\_{base}\left(s + \frac{N}{k + df_j}\right)\$\$

  `inversemax`

  : \$\$\textrm{log}\_{base}\left(s + \frac{\textrm{max}(df_j)}{k +
    df_j}\right)\$\$

  `inverseprob`

  : \$\$\textrm{log}\_{base}\left(\frac{N - df_j}{k + df_j}\right)\$\$

  `unary`

  : 1 for each feature

- base:

  the base with respect to which logarithms in the inverse document
  frequency weightings are computed; default is 10 (see Manning,
  Raghavan, and Schütze 2008, p123).

- smoothing:

  added to the quotient before taking the logarithm

- k:

  added to the denominator in the "inverse" weighting types, to prevent
  a zero document count for a term

- threshold:

  numeric value of the threshold *above which* a feature will considered
  in the computation of document frequency. The default is 0, meaning
  that a feature's document frequency will be the number of documents in
  which it occurs greater than zero times.

## Value

a numeric vector of document frequencies for each feature

## References

Manning, C. D., Raghavan, P., & Schütze, H. (2008). *Introduction to
Information Retrieval*. Cambridge: Cambridge University Press.
<https://nlp.stanford.edu/IR-book/pdf/irbookonlinereading.pdf>

## Examples

``` r
dfmat1 <- dfm(tokens(data_corpus_inaugural))
docfreq(dfmat1[, 1:20])
#> fellow-citizens              of             the          senate             and 
#>              19              60              60               9              60 
#>           house representatives               :           among    vicissitudes 
#>               8              14              38              43               5 
#>        incident              to            life              no           event 
#>               6              60              50              58               9 
#>           could            have          filled              me            with 
#>              35              60               5              47              59 

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
docfreq(dfmat2, scheme = "inverse")
#>    this      is       a  sample another example 
#> 0.00000 0.00000 0.30103 0.30103 0.30103 0.30103 
docfreq(dfmat2, scheme = "inverse", k = 1, smoothing = 1)
#>      this        is         a    sample   another   example 
#> 0.2218487 0.2218487 0.3010300 0.3010300 0.3010300 0.3010300 
docfreq(dfmat2, scheme = "unary")
#>    this      is       a  sample another example 
#>       1       1       1       1       1       1 
docfreq(dfmat2, scheme = "inversemax")
#>    this      is       a  sample another example 
#> 0.00000 0.00000 0.30103 0.30103 0.30103 0.30103 
docfreq(dfmat2, scheme = "inverseprob")
#>    this      is       a  sample another example 
#>       0       0       0       0       0       0 
```
