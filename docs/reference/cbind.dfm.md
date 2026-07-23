# Combine dfm objects by Rows or Columns

Combine a [dfm](https://quanteda.io/reference/dfm.md) with another dfm,
or numeric, or matrix object, returning a dfm with the combined
documents or features, respectively.

## Usage

``` r
# S3 method for class 'dfm'
cbind(...)

# S3 method for class 'dfm'
rbind(...)
```

## Arguments

- ...:

  [dfm](https://quanteda.io/reference/dfm.md), numeric, or matrix
  objects to be joined column-wise (`cbind`) or row-wise (`rbind`) to
  the first. Numeric objects not confirming to the row or column
  dimension will be recycled as normal.

## Details

`cbind(x, y, ...)` combines dfm objects by columns, returning a dfm
object with combined features from input dfm objects. Note that this
should be used with extreme caution, as joining dfms with different
documents will result in a new row with the docname(s) of the first dfm,
merging in those from the second. Furthermore, if features are shared
between the dfms being cbinded, then duplicate feature labels will
result. In both instances, warning messages will result.

`rbind(x, y, ...)` combines dfm objects by rows, returning a dfm object
with combined features from input dfm objects. Features are matched
between the two dfm objects, so that the order and names of the features
do not need to match. The order of the features in the resulting dfm is
not guaranteed. The attributes and settings of this new dfm are not
currently preserved.

## Examples

``` r
# cbind() for dfm objects
(dfmat1 <- dfm(tokens(c("a b c d", "c d e f"))))
#> Document-feature matrix of: 2 documents, 6 features (33.33% sparse) and 0
#> docvars.
#>        features
#> docs    a b c d e f
#>   text1 1 1 1 1 0 0
#>   text2 0 0 1 1 1 1
(dfmat2 <- dfm(tokens(c("a b", "x y z"))))
#> Document-feature matrix of: 2 documents, 5 features (50.00% sparse) and 0
#> docvars.
#>        features
#> docs    a b x y z
#>   text1 1 1 0 0 0
#>   text2 0 0 1 1 1
cbind(dfmat1, dfmat2)
#> Warning: cbinding dfms with overlapping features
#> Document-feature matrix of: 2 documents, 11 features (40.91% sparse) and 0
#> docvars.
#>        features
#> docs    a b c d e f a b x y
#>   text1 1 1 1 1 0 0 1 1 0 0
#>   text2 0 0 1 1 1 1 0 0 1 1
#> [ reached max_nfeat ... 1 more feature ]
cbind(dfmat1, 100)
#> Document-feature matrix of: 2 documents, 7 features (28.57% sparse) and 0
#> docvars.
#>        features
#> docs    a b c d e f feat1
#>   text1 1 1 1 1 0 0   100
#>   text2 0 0 1 1 1 1   100
cbind(100, dfmat1)
#> Document-feature matrix of: 2 documents, 7 features (28.57% sparse) and 0
#> docvars.
#>        features
#> docs    feat1 a b c d e f
#>   text1   100 1 1 1 1 0 0
#>   text2   100 0 0 1 1 1 1
cbind(dfmat1, matrix(c(101, 102), ncol = 1))
#> Document-feature matrix of: 2 documents, 7 features (28.57% sparse) and 0
#> docvars.
#>        features
#> docs    a b c d e f feat1
#>   text1 1 1 1 1 0 0   101
#>   text2 0 0 1 1 1 1   102
cbind(matrix(c(101, 102), ncol = 1), dfmat1)
#> Document-feature matrix of: 2 documents, 7 features (28.57% sparse) and 0
#> docvars.
#>        features
#> docs    feat1 a b c d e f
#>   text1   101 1 1 1 1 0 0
#>   text2   102 0 0 1 1 1 1


# rbind() for dfm objects
(dfmat1 <- dfm(tokens(c(doc1 = "This is one sample text sample."))))
#> Document-feature matrix of: 1 document, 6 features (0.00% sparse) and 0 docvars.
#>       features
#> docs   this is one sample text .
#>   doc1    1  1   1      2    1 1
(dfmat2 <- dfm(tokens(c(doc2 = "One two three text text."))))
#> Document-feature matrix of: 1 document, 5 features (0.00% sparse) and 0 docvars.
#>       features
#> docs   one two three text .
#>   doc2   1   1     1    2 1
(dfmat3 <- dfm(tokens(c(doc3 = "This is the fourth sample text."))))
#> Document-feature matrix of: 1 document, 7 features (0.00% sparse) and 0 docvars.
#>       features
#> docs   this is the fourth sample text .
#>   doc3    1  1   1      1      1    1 1
rbind(dfmat1, dfmat2)
#> Document-feature matrix of: 2 documents, 8 features (31.25% sparse) and 0
#> docvars.
#>       features
#> docs   this is one sample text . two three
#>   doc1    1  1   1      2    1 1   0     0
#>   doc2    0  0   1      0    2 1   1     1
rbind(dfmat1, dfmat2, dfmat3)
#> Document-feature matrix of: 3 documents, 10 features (40.00% sparse) and 0
#> docvars.
#>       features
#> docs   this is one sample text . two three the fourth
#>   doc1    1  1   1      2    1 1   0     0   0      0
#>   doc2    0  0   1      0    2 1   1     1   0      0
#>   doc3    1  1   0      1    1 1   0     0   1      1
```
