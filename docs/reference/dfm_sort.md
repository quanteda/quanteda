# Sort a dfm by frequency of one or more margins

Sorts a [dfm](https://quanteda.io/reference/dfm.md) by descending
frequency of total features, total features in documents, or both.

## Usage

``` r
dfm_sort(x, decreasing = TRUE, margin = c("features", "documents", "both"))
```

## Arguments

- x:

  Document-feature matrix created by
  [`dfm()`](https://quanteda.io/reference/dfm.md)

- decreasing:

  logical; if `TRUE`, the sort will be in descending order, otherwise
  sort in increasing order

- margin:

  which margin to sort on `features` to sort by frequency of features,
  `documents` to sort by total feature counts in documents, and `both`
  to sort by both

## Value

A sorted [dfm](https://quanteda.io/reference/dfm.md) matrix object

## Author

Ken Benoit

## Examples

``` r
dfmat <- dfm(tokens(data_corpus_inaugural))
head(dfmat)
#> Document-feature matrix of: 6 documents, 9,591 features (93.93% sparse) and 4
#> docvars.
#>                  features
#> docs              fellow-citizens  of the senate and house representatives :
#>   1789-Washington               1  71 116      1  48     2               2 1
#>   1793-Washington               0  11  13      0   2     0               0 1
#>   1797-Adams                    3 140 163      1 130     0               2 0
#>   1801-Jefferson                2 104 130      0  81     0               0 1
#>   1805-Jefferson                0 101 143      0  93     0               0 0
#>   1809-Madison                  1  69 104      0  43     0               0 0
#>                  features
#> docs              among vicissitudes
#>   1789-Washington     1            1
#>   1793-Washington     0            0
#>   1797-Adams          4            0
#>   1801-Jefferson      1            0
#>   1805-Jefferson      7            0
#>   1809-Madison        0            0
#> [ reached max_nfeat ... 9,581 more features ]
head(dfm_sort(dfmat))
#> Document-feature matrix of: 6 documents, 9,591 features (93.93% sparse) and 4
#> docvars.
#>                  features
#> docs              the   ,  of and  . to in  a our we
#>   1789-Washington 116  70  71  48 23 48 31 14   1  1
#>   1793-Washington  13   5  11   2  4  5  3  0   0  0
#>   1797-Adams      163 201 140 130 33 72 47 51   6  3
#>   1801-Jefferson  130 128 104  81 37 61 24 21  24 10
#>   1805-Jefferson  143 142 101  93 41 83 35 20  24 13
#>   1809-Madison    104  47  69  43 21 61 34 19   9  2
#> [ reached max_nfeat ... 9,581 more features ]
head(dfm_sort(dfmat, decreasing = FALSE, "both"))
#> Document-feature matrix of: 6 documents, 9,591 features (96.39% sparse) and 4
#> docvars.
#>                  features
#> docs              notification 14th fondest predilection flattering asylum
#>   1793-Washington            0    0       0            0          0      0
#>   1945-Roosevelt             0    0       0            0          0      0
#>   1865-Lincoln               0    0       0            0          0      0
#>   1905-Roosevelt             0    0       0            0          0      0
#>   1849-Taylor                0    0       0            0          0      0
#>   1829-Jackson               0    0       0            0          0      0
#>                  features
#> docs              interruptions awaken distrustful despondence
#>   1793-Washington             0      0           0           0
#>   1945-Roosevelt              0      0           0           0
#>   1865-Lincoln                0      0           0           0
#>   1905-Roosevelt              0      0           0           0
#>   1849-Taylor                 0      0           0           0
#>   1829-Jackson                0      0           0           0
#> [ reached max_nfeat ... 9,581 more features ]
```
