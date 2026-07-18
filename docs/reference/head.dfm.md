# Return the first or last part of a dfm

For a [dfm](https://quanteda.io/reference/dfm.md) object, return the dfm
with only the first or last `n` documents.

## Usage

``` r
# S3 method for class 'dfm'
head(x, n = 6L, ...)

# S3 method for class 'dfm'
tail(x, n = 6L, ...)
```

## Arguments

- x:

  a [dfm](https://quanteda.io/reference/dfm.md) object

- n:

  an integer vector of length up to `dim(x)` (or 1, for non-dimensioned
  objects). A `logical` is silently coerced to integer. Values specify
  the indices to be selected in the corresponding dimension (or along
  the length) of the object. A positive value of `n[i]` includes the
  first/last `n[i]` indices in that dimension, while a negative value
  excludes the last/first `abs(n[i])`, including all remaining indices.
  `NA` or non-specified values (when `length(n) < length(dim(x))`)
  select all indices in that dimension. Must contain at least one
  non-missing value.

- ...:

  arguments to be passed to or from other methods.

## Value

A [dfm](https://quanteda.io/reference/dfm.md) class object corresponding
to the subset of documents determined by by `n`.

## Examples

``` r
head(data_dfm_lbgexample, 3)
#> Document-feature matrix of: 3 documents, 37 features (54.05% sparse) and 0
#> docvars.
#>     features
#> docs A B  C  D  E  F   G   H   I   J
#>   R1 2 3 10 22 45 78 115 146 158 146
#>   R2 0 0  0  0  0  2   3  10  22  45
#>   R3 0 0  0  0  0  0   0   0   0   0
#> [ reached max_nfeat ... 27 more features ]
head(data_dfm_lbgexample, -4)
#> Document-feature matrix of: 2 documents, 37 features (54.05% sparse) and 0
#> docvars.
#>     features
#> docs A B  C  D  E  F   G   H   I   J
#>   R1 2 3 10 22 45 78 115 146 158 146
#>   R2 0 0  0  0  0  2   3  10  22  45
#> [ reached max_nfeat ... 27 more features ]

tail(data_dfm_lbgexample)
#> Document-feature matrix of: 6 documents, 37 features (54.05% sparse) and 0
#> docvars.
#>     features
#> docs A B  C  D  E  F   G   H   I   J
#>   R1 2 3 10 22 45 78 115 146 158 146
#>   R2 0 0  0  0  0  2   3  10  22  45
#>   R3 0 0  0  0  0  0   0   0   0   0
#>   R4 0 0  0  0  0  0   0   0   0   0
#>   R5 0 0  0  0  0  0   0   0   0   0
#>   V1 0 0  0  0  0  0   0   2   3  10
#> [ reached max_nfeat ... 27 more features ]
tail(data_dfm_lbgexample, n = 3)
#> Document-feature matrix of: 3 documents, 37 features (54.05% sparse) and 0
#> docvars.
#>     features
#> docs A B C D E F G H I  J
#>   R4 0 0 0 0 0 0 0 0 0  0
#>   R5 0 0 0 0 0 0 0 0 0  0
#>   V1 0 0 0 0 0 0 0 2 3 10
#> [ reached max_nfeat ... 27 more features ]
```
