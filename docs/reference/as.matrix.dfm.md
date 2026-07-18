# Coerce a dfm to a matrix or data.frame

Methods for coercing a [dfm](https://quanteda.io/reference/dfm.md)
object to a matrix or data.frame object.

## Usage

``` r
# S3 method for class 'dfm'
as.matrix(x, ...)
```

## Arguments

- x:

  dfm to be coerced

- ...:

  unused

## Examples

``` r
# coercion to matrix
as.matrix(data_dfm_lbgexample[, 1:10])
#>     features
#> docs A B  C  D  E  F   G   H   I   J
#>   R1 2 3 10 22 45 78 115 146 158 146
#>   R2 0 0  0  0  0  2   3  10  22  45
#>   R3 0 0  0  0  0  0   0   0   0   0
#>   R4 0 0  0  0  0  0   0   0   0   0
#>   R5 0 0  0  0  0  0   0   0   0   0
#>   V1 0 0  0  0  0  0   0   2   3  10
```
