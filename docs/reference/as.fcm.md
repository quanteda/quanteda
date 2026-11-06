# Coercion and checking functions for fcm objects

Convert an eligible input object into a fcm, or check whether an object
is a fcm. Current eligible inputs for coercion to a dfm are:
[matrix](https://rdrr.io/r/base/matrix.html), (sparse)
[Matrix](https://rdrr.io/pkg/Matrix/man/Matrix.html) and other
[fcm](https://quanteda.io/reference/fcm.md) objects.

## Usage

``` r
as.fcm(x)
```

## Arguments

- x:

  a candidate object for checking or coercion to
  [dfm](https://quanteda.io/reference/dfm.md)

## Value

`as.fcm` converts an input object into a
[fcm](https://quanteda.io/reference/fcm.md).
